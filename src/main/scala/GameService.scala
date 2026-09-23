import ConnectFour.*
import zio.*
import zio.stream.*

trait GameService:
  def start(request: StartGameRequest): IO[String, GameSnapshot]
  def get(id: String): UIO[Option[GameSnapshot]]
  def events(id: String, afterSequence: Long = 0L): UIO[Option[ZStream[Any, Nothing, GameEvent]]]
  def lastEvent(id: String): UIO[Option[GameEvent]]
  def cancel(id: String): UIO[Option[GameSnapshot]]

object GameService:
  private val MaxConcurrentGames = 4
  private val MaxUncachedStartsPerMinute = 8
  private val UncachedStartWindow = 1.minute
  private val MaxActiveSessions = 100
  private val MaxRetainedGames = 100

  private final case class CacheKey(modelId: String, firstPlayer: Player)
  private final case class CachedGame(events: Vector[GameEvent])

  def replayDelay(event: GameEvent, index: Int, turnPause: Duration): Duration =
    event.eventType match
      case GameEventType.TurnStart if index == 0 => Duration.Zero
      case GameEventType.TurnStart => turnPause
      case GameEventType.TurnEnd => Duration.fromMillis(event.game.moves.lastOption.fold(0L)(_.durationMs))

  val live: URLayer[MoveChooser & ModelAvailability, GameService] = layer(650.millis)

  def layer(
    turnPause: Duration,
    maxActiveSessions: Int = MaxActiveSessions,
    maxUncachedStartsPerWindow: Int = MaxUncachedStartsPerMinute,
  ): URLayer[MoveChooser & ModelAvailability, GameService] =
    ZLayer.fromZIO:
      for
        chooser <- ZIO.service[MoveChooser]
        availability <- ZIO.service[ModelAvailability]
        games <- Ref.make(Map.empty[String, GameSnapshot])
        sessions <- Ref.make(Map.empty[String, GameEventChannel])
        fibers <- Ref.make(Map.empty[String, Fiber.Runtime[Nothing, Unit]])
        cache <- Ref.make(Map.empty[CacheKey, CachedGame])
        inFlightKeys <- Ref.make(Set.empty[CacheKey])
        uncachedStartTimes <- Ref.make(Vector.empty[Long])
        cacheSemaphore <- Semaphore.make(1)
        activeSessions <- Ref.make(0)
        occupiedSlots <- Ref.make(0)
      yield Live(
        chooser,
        availability,
        games,
        sessions,
        fibers,
        cache,
        inFlightKeys,
        uncachedStartTimes,
        cacheSemaphore,
        activeSessions,
        occupiedSlots,
        turnPause,
        maxActiveSessions,
        maxUncachedStartsPerWindow,
      )

  private final case class Live(
    chooser: MoveChooser,
    availability: ModelAvailability,
    games: Ref[Map[String, GameSnapshot]],
    sessions: Ref[Map[String, GameEventChannel]],
    fibers: Ref[Map[String, Fiber.Runtime[Nothing, Unit]]],
    cache: Ref[Map[CacheKey, CachedGame]],
    inFlightKeys: Ref[Set[CacheKey]],
    uncachedStartTimes: Ref[Vector[Long]],
    cacheSemaphore: Semaphore,
    activeSessions: Ref[Int],
    occupiedSlots: Ref[Int],
    turnPause: Duration,
    maxActiveSessions: Int,
    maxUncachedStartsPerWindow: Int,
  ) extends GameService:

    def start(request: StartGameRequest): IO[String, GameSnapshot] =
      ZIO.uninterruptible:
        for
          model <- ZIO.fromOption(availability.findAvailable(request.modelId)).orElseFail(
            s"LLM model is not available for this account. Choose one of: ${availability.snapshot.available.map(_.id).mkString(", ")}"
          )
          first <- parseFirstPlayer(request.firstPlayer)
          key = CacheKey(model.id, first)
          cached <- cachedOrClaim(key)
          uncached = cached.isEmpty
          _ <- reserveSession.catchAll(error =>
            releaseClaim(key).when(uncached) *> ZIO.fail(error)
          )
          _ <- reserveSlot.when(uncached).catchAll(error =>
            releaseSession *> releaseClaim(key) *> ZIO.fail(error)
          )
          _ <- reserveUncachedStart.when(uncached).catchAll(error =>
            releaseSlot *> releaseSession *> releaseClaim(key) *> ZIO.fail(error)
          )
          id <- Random.nextUUID.map(_.toString)
          now <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
          initial = initialSnapshot(id, now, model, first, cached.isDefined)
          session <- GameEventChannel.make
          _ <- register(initial, session)
          work = cached match
            case Some(value) =>
              ZIO.logInfo(s"Replaying cached game: model=${model.id}, firstPlayer=${first.label}, gameId=$id") *>
                replay(initial, value, session)
            case None =>
              ZIO.logInfo(s"Computing uncached game: model=${model.id}, firstPlayer=${first.label}, gameId=$id") *>
                runGame(initial, Board.empty, first, model, key, session)
                  .catchAll(error => failGame(id, error, session))
          _ <- launch(id, work, Option.when(uncached)(key))
        yield initial

    def get(id: String): UIO[Option[GameSnapshot]] =
      games.get.map(_.get(id))

    def events(id: String, afterSequence: Long): UIO[Option[ZStream[Any, Nothing, GameEvent]]] =
      sessions.get.map(_.get(id).map(_.stream(afterSequence)))

    def lastEvent(id: String): UIO[Option[GameEvent]] =
      sessions.get.map(_.get(id)).flatMap:
        case Some(channel) => channel.events.map(_.lastOption)
        case None => ZIO.none

    def cancel(id: String): UIO[Option[GameSnapshot]] =
      for
        channel <- sessions.get.map(_.get(id))
        transitioned <- channel match
          case None => ZIO.none
          case Some(events) =>
            Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS).flatMap: now =>
              events.transition(
                GameEventType.TurnEnd,
                games.modify: all =>
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
                    case _ => None -> all,
              )(_ => ZIO.unit)
        result <- transitioned match
          case some @ Some(_) => ZIO.succeed(some)
          case None => games.get.map(_.get(id))
        maybeFiber <- fibers.get.map(_.get(id))
        _ <- ZIO.foreachDiscard(maybeFiber)(_.interrupt).when(transitioned.nonEmpty)
      yield result

    private def cachedOrClaim(key: CacheKey): IO[String, Option[CachedGame]] =
      cacheSemaphore.withPermit:
        cache.get.flatMap: cachedGames =>
          cachedGames.get(key) match
            case cached @ Some(_) => ZIO.succeed(cached)
            case None =>
              inFlightKeys.modify: keys =>
                if keys.contains(key) then
                  Left(s"An uncached ${key.modelId}/${key.firstPlayer.label}-first game is already running") -> keys
                else Right(None) -> (keys + key)
              .flatMap(ZIO.fromEither)

    private def releaseClaim(key: CacheKey): UIO[Unit] =
      inFlightKeys.update(_ - key)

    private def reserveUncachedStart: IO[String, Unit] =
      Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS).flatMap: now =>
        uncachedStartTimes.modify: starts =>
          val recent = starts.filter(_ > now - UncachedStartWindow.toMillis)
          if recent.size >= maxUncachedStartsPerWindow then
            Left(s"At most $maxUncachedStartsPerWindow uncached games may start per minute") -> recent
          else Right(()) -> (recent :+ now)
        .flatMap(ZIO.fromEither)

    private def initialSnapshot(
      id: String,
      now: Long,
      model: BedrockModelCatalog.Model,
      first: Player,
      cachedReplay: Boolean,
    ): GameSnapshot =
      GameSnapshot(
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
        message =
          if cachedReplay then s"Replaying cached ${first.label}-first game"
          else s"${first.label} is choosing the opening move",
        createdAtMs = now,
        turnStartedAtMs = Some(now),
        finishedAtMs = None,
        cachedReplay = cachedReplay,
      )

    private def reserveSession: IO[String, Unit] =
      activeSessions.modify: count =>
        if count >= maxActiveSessions then
          Left(s"At most $maxActiveSessions active game sessions may run at once") -> count
        else Right(()) -> (count + 1)
      .flatMap(ZIO.fromEither)

    private def releaseSession: UIO[Unit] =
      activeSessions.update(count => math.max(0, count - 1))

    private def reserveSlot: IO[String, Unit] =
      occupiedSlots.modify: count =>
        if count >= MaxConcurrentGames then
          Left(s"At most $MaxConcurrentGames games may run at once") -> count
        else Right(()) -> (count + 1)
      .flatMap(ZIO.fromEither)

    private def releaseSlot: UIO[Unit] =
      occupiedSlots.update(count => math.max(0, count - 1))

    private def register(initial: GameSnapshot, channel: GameEventChannel): UIO[Unit] =
      for
        removed <- games.modify: all =>
          val removeCount = math.max(0, all.size - MaxRetainedGames + 1)
          val ids = all.valuesIterator
            .filter(_.status != GameStatus.Thinking)
            .toVector
            .sortBy(_.createdAtMs)
            .take(removeCount)
            .map(_.id)
          ids -> ((all -- ids).updated(initial.id, initial))
        _ <- sessions.update(all => (all -- removed).updated(initial.id, channel))
      yield ()

    private def launch(id: String, work: UIO[Unit], uncachedKey: Option[CacheKey]): UIO[Unit] =
      for
        begin <- Promise.make[Nothing, Unit]
        cleanup = fibers.update(_ - id) *>
          releaseSession *>
          releaseSlot.when(uncachedKey.nonEmpty) *>
          ZIO.foreachDiscard(uncachedKey)(releaseClaim)
        fiber <- (begin.await *> work.interruptible).ensuring(cleanup).forkDaemon
        _ <- fibers.update(_.updated(id, fiber))
        _ <- begin.succeed(())
      yield ()

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
      key: CacheKey,
      channel: GameEventChannel,
    ): Task[Unit] =
      for
        turnStarted <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
        started <- channel.transition(
          GameEventType.TurnStart,
          games.modify: all =>
            all.get(snapshot.id) match
              case Some(current)
                  if current.status == GameStatus.Thinking && current.moves.size == snapshot.moves.size =>
                val value = current.copy(
                  currentPlayer = Some(player),
                  message = s"${player.label} is thinking…",
                  turnStartedAtMs = Some(turnStarted),
                )
                Some(value) -> all.updated(snapshot.id, value)
              case _ => None -> all,
        )(_ => ZIO.unit)
        _ <- ZIO.foreachDiscard(started): current =>
          for
            nanoStart <- Clock.nanoTime
            selection <- chooser.choose(player, board, model.id).tapError:
              case failure: MoveChooser.TurnFailure =>
                Clock.nanoTime.flatMap: failedAt =>
                  val failedDurationMs = math.max(0L, (failedAt - nanoStart) / 1000000L)
                  recordFailedTurn(
                    snapshot.id,
                    current.moves.size,
                    player,
                    model,
                    failedDurationMs,
                    failure,
                  )
              case _ => ZIO.unit
            nanoEnd <- Clock.nanoTime
            durationMs = math.max(0L, (nanoEnd - nanoStart) / 1000000L)
            played <- ZIO.fromEither(board.play(selection.column, player)).mapError(IllegalStateException(_))
            (nextBoard, row) = played
            moveCost = player match
              case Player.Jev => JevPricing.pricing.estimateUsd(selection.inputTokens, selection.outputTokens)
              case Player.Llm => model.pricing.estimateUsd(selection.inputTokens, selection.outputTokens)
            move = MoveRecord(
              turn = current.moves.size + 1,
              player = player,
              column = Some(selection.column),
              row = Some(row),
              outcome = MoveOutcome.Played,
              durationMs = durationMs,
              note = selection.note,
              inputTokens = selection.inputTokens,
              outputTokens = selection.outputTokens,
              estimatedCostUsd = moveCost,
              requestDetails = selection.requestDetails,
              responseDetails = selection.responseDetails,
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
            nextSnapshot = current.copy(
              board = nextBoard.view,
              status = status,
              currentPlayer = nextPlayer,
              winner = winner,
              moves = current.moves :+ move,
              bedrockCostUsd = current.bedrockCostUsd + (if player == Player.Llm then moveCost else BigDecimal(0)),
              jevCostUsd = current.jevCostUsd + (if player == Player.Jev then moveCost else BigDecimal(0)),
              message = message,
              turnStartedAtMs = None,
              finishedAtMs = if status == GameStatus.Thinking then None else Some(finishedAt),
            )
            committed <- cacheSemaphore.withPermit:
              channel.transition(
                GameEventType.TurnEnd,
                games.modify: all =>
                  all.get(snapshot.id) match
                    case Some(latest)
                        if latest.status == GameStatus.Thinking && latest.moves.size == current.moves.size =>
                      Some(nextSnapshot) -> all.updated(snapshot.id, nextSnapshot)
                    case _ => None -> all,
              )(completed => cacheCompleted(key, channel, completed).when(isCacheable(completed.status)).unit)
            _ <- (runGame(nextSnapshot, nextBoard, player.opponent, model, key, channel).delay(turnPause))
              .when(committed.nonEmpty && status == GameStatus.Thinking)
          yield ()
      yield ()

    private def isCacheable(status: GameStatus): Boolean = status match
      case GameStatus.Won | GameStatus.Draw => true
      case GameStatus.Thinking | GameStatus.Failed | GameStatus.Cancelled => false

    private def cacheCompleted(
      key: CacheKey,
      channel: GameEventChannel,
      completed: GameSnapshot,
    ): UIO[Unit] =
      channel.events.flatMap: events =>
        val completedEvent = GameEvent(events.size.toLong + 1L, GameEventType.TurnEnd, completed)
        val cachedEvents = events :+ completedEvent
        cache.update(_.updated(key, CachedGame(cachedEvents))) *>
          ZIO.logInfo(s"Cached completed game: model=${key.modelId}, firstPlayer=${key.firstPlayer.label}, events=${cachedEvents.size}")

    private def replay(
      initial: GameSnapshot,
      cached: CachedGame,
      channel: GameEventChannel,
    ): UIO[Unit] =
      ZIO.foreachDiscard(cached.events.zipWithIndex): (event, index) =>
        val delay = GameService.replayDelay(event, index, turnPause)
        ZIO.sleep(delay) *>
          Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS).flatMap: now =>
            val replayed = event.game.copy(
              id = initial.id,
              createdAtMs = initial.createdAtMs,
              turnStartedAtMs = Option.when(event.eventType == GameEventType.TurnStart)(now),
              finishedAtMs = Option.when(event.game.status != GameStatus.Thinking)(now),
              cachedReplay = true,
            )
            channel.transition(
              event.eventType,
              games.modify: all =>
                all.get(initial.id) match
                  case Some(current) if current.status == GameStatus.Thinking =>
                    Some(replayed) -> all.updated(initial.id, replayed)
                  case _ => None -> all,
            )(_ => ZIO.unit).unit

    private def recordFailedTurn(
      id: String,
      expectedMoveCount: Int,
      player: Player,
      model: BedrockModelCatalog.Model,
      durationMs: Long,
      failure: MoveChooser.TurnFailure,
    ): UIO[Unit] =
      val cost = player match
        case Player.Jev => JevPricing.pricing.estimateUsd(failure.inputTokens, failure.outputTokens)
        case Player.Llm => model.pricing.estimateUsd(failure.inputTokens, failure.outputTokens)
      games.update: all =>
        all.get(id) match
          case Some(current)
              if current.status == GameStatus.Thinking && current.moves.size == expectedMoveCount =>
            val rejected = MoveRecord(
              turn = current.moves.size + 1,
              player = player,
              column = failure.attemptedColumn,
              row = None,
              outcome = MoveOutcome.Rejected,
              durationMs = durationMs,
              note = failure.getMessage,
              inputTokens = failure.inputTokens,
              outputTokens = failure.outputTokens,
              estimatedCostUsd = cost,
              requestDetails = failure.requestDetails,
              responseDetails = failure.responseDetails,
            )
            all.updated(id, current.copy(
              moves = current.moves :+ rejected,
              bedrockCostUsd = current.bedrockCostUsd + (if player == Player.Llm then cost else BigDecimal(0)),
              jevCostUsd = current.jevCostUsd + (if player == Player.Jev then cost else BigDecimal(0)),
            ))
          case _ => all

    private def failGame(id: String, error: Throwable, channel: GameEventChannel): UIO[Unit] =
      Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS).flatMap: now =>
        channel.transition(
          GameEventType.TurnEnd,
          games.modify: all =>
            all.get(id) match
              case Some(current) if current.status == GameStatus.Thinking =>
                val value = current.copy(
                  status = GameStatus.Failed,
                  currentPlayer = None,
                  message = Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName),
                  turnStartedAtMs = None,
                  finishedAtMs = Some(now),
                )
                Some(value) -> all.updated(id, value)
              case _ => None -> all,
        )(_ => ZIO.unit).unit
