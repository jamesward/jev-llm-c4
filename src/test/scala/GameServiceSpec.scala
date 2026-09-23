import ConnectFour.*
import zio.*
import zio.test.*

object GameServiceSpec extends ZIOSpecDefault:
  private val ModelId = BedrockModelCatalog.Glm47Flash.id

  private def withService[A](
    chooser: MoveChooser,
    maxUncachedStartsPerWindow: Int = 8,
  )(
    effect: ZIO[GameService, String, A]
  ): IO[String, A] =
    val dependencies = ZLayer.succeed(chooser) ++ ModelAvailability.allCatalog
    effect.provideLayer(dependencies >>> GameService.layer(
      Duration.Zero,
      maxUncachedStartsPerWindow = maxUncachedStartsPerWindow,
    ))

  def spec = suite("GameService")(
    test("runs alternating players to a detected win and accumulates provider costs") {
      val chooser = new MoveChooser:
        def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] =
          if player == Player.Jev then ZIO.succeed(MoveChooser.Selection(0, "scripted", 500, 50, "jev request", "jev response"))
          else ZIO.succeed(MoveChooser.Selection(1, "scripted", 1000, 100, "llm request", "llm response"))

      withService(chooser):
        for
          service <- ZIO.service[GameService]
          started <- service.start(StartGameRequest(ModelId, "jev"))
          finished <- {
            def awaitResult: UIO[GameSnapshot] =
              service.get(started.id).flatMap:
                case Some(game) if game.status != GameStatus.Thinking => ZIO.succeed(game)
                case _ => ZIO.yieldNow *> awaitResult
            awaitResult
          }
          expectedBedrockMoveCost = BedrockModelCatalog.Glm47Flash.pricing.estimateUsd(1000, 100)
          expectedJevMoveCost = JevPricing.pricing.estimateUsd(500, 50)
        yield assertTrue(
          finished.status == GameStatus.Won,
          finished.winner.contains(Player.Jev),
          finished.moves.size == 7,
          finished.moves.filter(_.player == Player.Jev).forall(_.estimatedCostUsd == expectedJevMoveCost),
          finished.moves.filter(_.player == Player.Llm).forall(_.estimatedCostUsd == expectedBedrockMoveCost),
          finished.jevCostUsd == expectedJevMoveCost * 4,
          finished.bedrockCostUsd == expectedBedrockMoveCost * 3,
          finished.moves.filter(_.player == Player.Jev).forall(move => move.requestDetails == "jev request" && move.responseDetails == "jev response"),
          finished.moves.filter(_.player == Player.Llm).forall(move => move.requestDetails == "llm request" && move.responseDetails == "llm response"),
        )
    } @@ TestAspect.timeout(5.seconds),
    test("records an illegal Jev choice in history without mutating the board") {
      val failure = MoveChooser.JevTurnFailure(
        details = "Jev selected illegal column 3",
        attemptedColumn = Some(3),
        inputTokens = 700,
        outputTokens = 82,
        requestDetails = "jev request json",
        responseDetails = "jev response json",
      )
      val chooser = new MoveChooser:
        def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] =
          ZIO.fail(failure)

      withService(chooser):
        for
          service <- ZIO.service[GameService]
          started <- service.start(StartGameRequest(ModelId, "jev"))
          failed <- {
            def awaitResult: UIO[GameSnapshot] = service.get(started.id).flatMap:
              case Some(game) if game.status != GameStatus.Thinking => ZIO.succeed(game)
              case _ => ZIO.yieldNow *> awaitResult
            awaitResult
          }
          rejected = failed.moves.headOption
        yield assertTrue(
          failed.status == GameStatus.Failed,
          failed.board == Board.empty.view,
          failed.message.contains("Jev selected illegal column 3"),
          failed.moves.size == 1,
          rejected.exists(_.outcome == MoveOutcome.Rejected),
          rejected.flatMap(_.column).contains(3),
          rejected.flatMap(_.row).isEmpty,
          rejected.exists(_.note == failure.getMessage),
          rejected.exists(_.inputTokens == 700),
          rejected.exists(_.outputTokens == 82),
          rejected.exists(_.requestDetails == "jev request json"),
          rejected.exists(_.responseDetails == "jev response json"),
          failed.jevCostUsd == JevPricing.pricing.estimateUsd(700, 82),
        )
    } @@ TestAspect.timeout(5.seconds),
    test("rejects model IDs outside the selectable catalog before invoking a player") {
      val chooser = new MoveChooser:
        def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] = ZIO.dieMessage("must not run")

      withService(chooser):
        for
          service <- ZIO.service[GameService]
          unknown <- service.start(StartGameRequest("custom.model", "jev")).exit
          unsupported <- service.start(StartGameRequest(BedrockModelCatalog.NovaMicro.id, "jev")).exit
        yield assertTrue(
          unknown.isFailure,
          unknown.causeOption.flatMap(_.failureOption).exists(_.startsWith("LLM model is not available")),
          unsupported.isFailure,
          unsupported.causeOption.flatMap(_.failureOption).exists(_.startsWith("LLM model is not available")),
        )
    },
    test("records invalid Bedrock response cost and identifies the model") {
      val model = BedrockModelCatalog.Glm47Flash
      val chooser = new MoveChooser:
        def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] =
          ZIO.fail(MoveChooser.BedrockTurnFailure(
            model.label,
            model.id,
            "returned invalid move JSON: test response",
            inputTokens = 1000,
            outputTokens = 100,
            requestDetails = "llm request",
            responseDetails = "llm response",
          ))

      withService(chooser):
        for
          service <- ZIO.service[GameService]
          started <- service.start(StartGameRequest(model.id, "llm"))
          failed <- {
            def awaitResult: UIO[GameSnapshot] = service.get(started.id).flatMap:
              case Some(game) if game.status != GameStatus.Thinking => ZIO.succeed(game)
              case _ => ZIO.yieldNow *> awaitResult
            awaitResult
          }
        yield assertTrue(
          failed.status == GameStatus.Failed,
          failed.bedrockCostUsd == model.pricing.estimateUsd(1000, 100),
          failed.message.contains(model.label),
          failed.message.contains(model.id),
          failed.message.contains("invalid move JSON"),
          failed.moves.size == 1,
          failed.moves.headOption.exists(_.outcome == MoveOutcome.Rejected),
          failed.moves.headOption.flatMap(_.column).isEmpty,
          failed.moves.headOption.flatMap(_.row).isEmpty,
          failed.moves.headOption.exists(_.requestDetails == "llm request"),
          failed.moves.headOption.exists(_.responseDetails == "llm response"),
        )
    } @@ TestAspect.timeout(5.seconds),
    test("cancellation cannot be overwritten by an in-flight move") {
      for
        choosing <- Promise.make[Nothing, Unit]
        neverFinish <- Promise.make[Nothing, MoveChooser.Selection]
        chooser = new MoveChooser:
          def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] =
            choosing.succeed(()) *> neverFinish.await
        result <- withService(chooser):
          for
            service <- ZIO.service[GameService]
            started <- service.start(StartGameRequest(ModelId, "jev"))
            _ <- choosing.await
            cancelled <- service.cancel(started.id)
            stored <- service.get(started.id)
          yield (cancelled, stored)
      yield assertTrue(
        result._1.exists(_.status == GameStatus.Cancelled),
        result._2.exists(_.status == GameStatus.Cancelled),
        result._2.exists(_.moves.isEmpty),
      )
    } @@ TestAspect.timeout(5.seconds),
    test("allows only one uncached computation per model and opening player") {
      val chooser = new MoveChooser:
        def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] = ZIO.never

      withService(chooser):
        for
          service <- ZIO.service[GameService]
          first <- service.start(StartGameRequest(ModelId, "jev"))
          duplicate <- service.start(StartGameRequest(ModelId, "jev")).exit
          _ <- service.cancel(first.id)
        yield assertTrue(
          duplicate.isFailure,
          duplicate.causeOption.flatMap(_.failureOption).exists(_.contains("already running")),
        )
    } @@ TestAspect.timeout(5.seconds),
    test("limits the rate of new uncached games") {
      val chooser = new MoveChooser:
        def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] =
          ZIO.fail(RuntimeException("scripted provider failure"))

      withService(chooser, maxUncachedStartsPerWindow = 2):
        for
          service <- ZIO.service[GameService]
          first <- service.start(StartGameRequest(ModelId, "jev"))
          _ <- {
            def awaitFailure: UIO[Unit] = service.get(first.id).flatMap:
              case Some(game) if game.status == GameStatus.Failed => ZIO.unit
              case _ => ZIO.yieldNow *> awaitFailure
            awaitFailure
          }
          second <- service.start(StartGameRequest(BedrockModelCatalog.Qwen332B.id, "jev"))
          _ <- {
            def awaitFailure: UIO[Unit] = service.get(second.id).flatMap:
              case Some(game) if game.status == GameStatus.Failed => ZIO.unit
              case _ => ZIO.yieldNow *> awaitFailure
            awaitFailure
          }
          third <- service.start(StartGameRequest(BedrockModelCatalog.MiniMaxM25.id, "jev")).exit
        yield assertTrue(
          third.isFailure,
          third.causeOption.flatMap(_.failureOption).contains("At most 2 uncached games may start per minute"),
        )
    } @@ TestAspect.timeout(5.seconds),
    test("limits concurrent games that can spend credentials") {
      val chooser = new MoveChooser:
        def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] = ZIO.never

      withService(chooser):
        for
          service <- ZIO.service[GameService]
          requests = Vector(
            StartGameRequest(ModelId, "jev"),
            StartGameRequest(ModelId, "llm"),
            StartGameRequest(BedrockModelCatalog.Qwen332B.id, "jev"),
            StartGameRequest(BedrockModelCatalog.Qwen332B.id, "llm"),
          )
          running <- ZIO.foreach(requests)(service.start)
          fifth <- service.start(StartGameRequest(BedrockModelCatalog.MiniMaxM25.id, "jev")).exit
          _ <- ZIO.foreachDiscard(running)(game => service.cancel(game.id))
        yield assertTrue(
          fifth.isFailure,
          fifth.causeOption.exists(_.failureOption.contains("At most 4 games may run at once")),
        )
    } @@ TestAspect.timeout(5.seconds),
    test("holds an admission slot until a cancelled fiber actually exits") {
      for
        enteredCount <- Ref.make(0)
        allEntered <- Promise.make[Nothing, Unit]
        release <- Promise.make[Nothing, Unit]
        chooser = new MoveChooser:
          def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] =
            enteredCount.updateAndGet(_ + 1).flatMap(count => allEntered.succeed(()).when(count == 4)) *>
              ZIO.uninterruptible(release.await) *>
              ZIO.succeed(MoveChooser.Selection(0, "released"))
        result <- withService(chooser):
          for
            service <- ZIO.service[GameService]
            requests = Vector(
              StartGameRequest(ModelId, "jev"),
              StartGameRequest(ModelId, "llm"),
              StartGameRequest(BedrockModelCatalog.Qwen332B.id, "jev"),
              StartGameRequest(BedrockModelCatalog.Qwen332B.id, "llm"),
            )
            running <- ZIO.foreach(requests)(service.start)
            _ <- allEntered.await
            cancelling <- service.cancel(running.head.id).fork
            _ <- {
              def awaitCancelled: UIO[Unit] = service.get(running.head.id).flatMap:
                case Some(game) if game.status == GameStatus.Cancelled => ZIO.unit
                case _ => ZIO.yieldNow *> awaitCancelled
              awaitCancelled
            }
            overlappingStart <- service.start(StartGameRequest(BedrockModelCatalog.MiniMaxM25.id, "jev")).exit
            _ <- release.succeed(())
            _ <- cancelling.join
            _ <- ZIO.foreachDiscard(running.tail)(game => service.cancel(game.id))
          yield overlappingStart
      yield assertTrue(
        result.isFailure,
        result.causeOption.exists(_.failureOption.contains("At most 4 games may run at once")),
      )
    } @@ TestAspect.timeout(5.seconds),
  ) @@ TestAspect.sequential
