import ConnectFour.*
import zio.*

trait GameService:
  def start(request: StartGameRequest): IO[String, GameSnapshot]
  def get(id: String): UIO[Option[GameSnapshot]]
  def cancel(id: String): UIO[Option[GameSnapshot]]

object GameService:
  private val MaxConcurrentGames = 4
  private val MaxRetainedGames = 100

  val live: URLayer[MoveChooser & ModelAvailability, GameService] = layer(650.millis)

  def layer(turnPause: Duration): URLayer[MoveChooser & ModelAvailability, GameService] =
    ZLayer.fromZIO:
      for
        chooser <- ZIO.service[MoveChooser]
        availability <- ZIO.service[ModelAvailability]
        games <- Ref.make(Map.empty[String, GameSnapshot])
        fibers <- Ref.make(Map.empty[String, Fiber.Runtime[Nothing, Unit]])
        occupiedSlots <- Ref.make(0)
      yield Live(chooser, availability, games, fibers, occupiedSlots, turnPause)

  private final case class Live(
    chooser: MoveChooser,
    availability: ModelAvailability,
    games: Ref[Map[String, GameSnapshot]],
    fibers: Ref[Map[String, Fiber.Runtime[Nothing, Unit]]],
    occupiedSlots: Ref[Int],
    turnPause: Duration,
  ) extends GameService:

    def start(request: StartGameRequest): IO[String, GameSnapshot] =
      for
        model <- ZIO.fromOption(availability.findAvailable(request.modelId)).orElseFail(
          s"Bedrock model is not available for this account. Choose one of: ${availability.snapshot.available.map(_.id).mkString(", ")}"
        )
        first <- parseFirstPlayer(request.firstPlayer)
        id <- Random.nextUUID.map(_.toString)
        now <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
        initial = GameSnapshot(
          id = id,
          modelId = model.id,
          modelLabel = model.label,
          inputUsdPerMillion = model.pricing.inputUsdPerMillion,
          outputUsdPerMillion = model.pricing.outputUsdPerMillion,
          bedrockCostUsd = BigDecimal(0),
          jevInputUsdPerMillion = JevPricing.InputUsdPerMillion,
          jevOutputUsdPerMillion = JevPricing.OutputUsdPerMillion,
          jevCostUsd = BigDecimal(0),
          board = Board.empty.view,
          status = GameStatus.Thinking,
          currentPlayer = Some(first),
          winner = None,
          moves = Vector.empty,
          message = s"${first.label} is choosing the opening move",
          createdAtMs = now,
          turnStartedAtMs = Some(now),
          finishedAtMs = None,
        )
        reserved <- occupiedSlots.modify: count =>
          if count >= MaxConcurrentGames then
            Left(s"At most $MaxConcurrentGames games may run at once") -> count
          else Right(()) -> (count + 1)
        _ <- ZIO.fromEither(reserved)
        _ <- games.update: all =>
          val removeCount = math.max(0, all.size - MaxRetainedGames + 1)
          val oldestFinished = all.valuesIterator
            .filter(_.status != GameStatus.Thinking)
            .toVector
            .sortBy(_.createdAtMs)
            .take(removeCount)
            .map(_.id)
          (all -- oldestFinished).updated(id, initial)
        fiber <- runGame(initial, Board.empty, first, model)
          .catchAll(error => markFailed(id, error))
          .ensuring(fibers.update(_ - id) *> occupiedSlots.update(count => math.max(0, count - 1)))
          .forkDaemon
        _ <- fibers.update(_.updated(id, fiber))
      yield initial

    def get(id: String): UIO[Option[GameSnapshot]] =
      games.get.map(_.get(id))

    def cancel(id: String): UIO[Option[GameSnapshot]] =
      for
        now <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
        updated <- games.modify: all =>
          all.get(id) match
            case Some(game) if game.status == GameStatus.Thinking =>
              val cancelled = game.copy(
                status = GameStatus.Cancelled,
                currentPlayer = None,
                message = "Game cancelled",
                turnStartedAtMs = None,
                finishedAtMs = Some(now),
              )
              Some(cancelled) -> all.updated(id, cancelled)
            case other => other -> all
        maybeFiber <- fibers.get.map(_.get(id))
        _ <- ZIO.foreachDiscard(maybeFiber)(_.interrupt)
      yield updated

    private def parseFirstPlayer(raw: String): IO[String, Player] =
      raw.trim.toLowerCase match
        case "jev" => ZIO.succeed(Player.Jev)
        case "llm" => ZIO.succeed(Player.Llm)
        case other => ZIO.fail(s"Unknown first player '$other'; expected jev or llm")

    private def runGame(
      snapshot: GameSnapshot,
      board: Board,
      player: Player,
      model: BedrockModelCatalog.Model,
    ): Task[Unit] =
      for
        turnStarted <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
        started <- games.modify: all =>
          all.get(snapshot.id) match
            case Some(current)
                if current.status == GameStatus.Thinking && current.moves.size == snapshot.moves.size =>
              true -> all.updated(snapshot.id, current.copy(
                currentPlayer = Some(player),
                message = s"${player.label} is thinking…",
                turnStartedAtMs = Some(turnStarted),
              ))
            case _ => false -> all
        _ <- ZIO.when(started):
          for
            nanoStart <- Clock.nanoTime
            selection <- chooser.choose(player, board, model.id).tapError:
              case failure: MoveChooser.BedrockTurnFailure =>
                recordFailedBedrockCost(snapshot.id, model, failure)
              case _ => ZIO.unit
            nanoEnd <- Clock.nanoTime
            durationMs = math.max(0L, (nanoEnd - nanoStart) / 1000000L)
            played <- ZIO.fromEither(board.play(selection.column, player)).mapError(IllegalStateException(_))
            (nextBoard, row) = played
            moveCost = player match
              case Player.Jev => JevPricing.pricing.estimateUsd(selection.inputTokens, selection.outputTokens)
              case Player.Llm => model.pricing.estimateUsd(selection.inputTokens, selection.outputTokens)
            move = MoveRecord(
              snapshot.moves.size + 1,
              player,
              selection.column,
              row,
              durationMs,
              selection.note,
              selection.inputTokens,
              selection.outputTokens,
              moveCost,
            )
            finishedAt <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
            winner = nextBoard.winner
            isDraw = winner.isEmpty && nextBoard.isFull
            status = winner.fold(if isDraw then GameStatus.Draw else GameStatus.Thinking)(_ => GameStatus.Won)
            nextPlayer = if status == GameStatus.Thinking then Some(player.opponent) else None
            message = winner match
              case Some(value) => s"${value.label} wins in ${move.turn} moves"
              case None if isDraw => "Draw — the board is full"
              case None => s"${player.label} played column ${selection.column}"
            nextSnapshot = snapshot.copy(
              board = nextBoard.view,
              status = status,
              currentPlayer = nextPlayer,
              winner = winner,
              moves = snapshot.moves :+ move,
              bedrockCostUsd = snapshot.bedrockCostUsd + (if player == Player.Llm then moveCost else BigDecimal(0)),
              jevCostUsd = snapshot.jevCostUsd + (if player == Player.Jev then moveCost else BigDecimal(0)),
              message = message,
              turnStartedAtMs = None,
              finishedAtMs = if status == GameStatus.Thinking then None else Some(finishedAt),
            )
            committed <- games.modify: all =>
              all.get(snapshot.id) match
                case Some(current)
                    if current.status == GameStatus.Thinking && current.moves.size == snapshot.moves.size =>
                  true -> all.updated(snapshot.id, nextSnapshot)
                case _ => false -> all
            _ <- ZIO.when(committed && status == GameStatus.Thinking):
              ZIO.sleep(turnPause) *> runGame(nextSnapshot, nextBoard, player.opponent, model)
          yield ()
      yield ()

    private def recordFailedBedrockCost(
      id: String,
      model: BedrockModelCatalog.Model,
      failure: MoveChooser.BedrockTurnFailure,
    ): UIO[Unit] =
      val cost = model.pricing.estimateUsd(failure.inputTokens, failure.outputTokens)
      games.update: all =>
        all.get(id).fold(all): current =>
          if current.status != GameStatus.Thinking then all
          else all.updated(id, current.copy(bedrockCostUsd = current.bedrockCostUsd + cost))

    private def markFailed(id: String, error: Throwable): UIO[Unit] =
      for
        now <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
        _ <- games.update: all =>
          all.get(id).fold(all): current =>
            if current.status != GameStatus.Thinking then all
            else all.updated(id, current.copy(
              status = GameStatus.Failed,
              currentPlayer = None,
              message = Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName),
              turnStartedAtMs = None,
              finishedAtMs = Some(now),
            ))
      yield ()
