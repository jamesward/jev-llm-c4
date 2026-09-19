import ConnectFour.*
import zio.*
import zio.test.*

object GameServiceSpec extends ZIOSpecDefault:
  private val ModelId = BedrockModelCatalog.Llama4Maverick.id

  private def withService[A](chooser: MoveChooser)(effect: ZIO[GameService, String, A]): IO[String, A] =
    val dependencies = ZLayer.succeed(chooser) ++ ModelAvailability.allCatalog
    effect.provideLayer(dependencies >>> GameService.layer(Duration.Zero))

  def spec = suite("GameService")(
    test("runs alternating players to a detected win and accumulates provider costs") {
      val chooser = new MoveChooser:
        def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] =
          if player == Player.Jev then ZIO.succeed(MoveChooser.Selection(0, "scripted", 500, 50))
          else ZIO.succeed(MoveChooser.Selection(1, "scripted", 1000, 100))

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
          expectedBedrockMoveCost = BedrockModelCatalog.Llama4Maverick.pricing.estimateUsd(1000, 100)
          expectedJevMoveCost = JevPricing.pricing.estimateUsd(500, 50)
        yield assertTrue(
          finished.status == GameStatus.Won,
          finished.winner.contains(Player.Jev),
          finished.moves.size == 7,
          finished.moves.filter(_.player == Player.Jev).forall(_.estimatedCostUsd == expectedJevMoveCost),
          finished.moves.filter(_.player == Player.Llm).forall(_.estimatedCostUsd == expectedBedrockMoveCost),
          finished.jevCostUsd == expectedJevMoveCost * 4,
          finished.bedrockCostUsd == expectedBedrockMoveCost * 3,
        )
    } @@ TestAspect.timeout(5.seconds),
    test("rejects model IDs outside the fixed catalog before invoking a player") {
      val chooser = new MoveChooser:
        def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] = ZIO.dieMessage("must not run")

      withService(chooser):
        for
          service <- ZIO.service[GameService]
          result <- service.start(StartGameRequest("custom.model", "jev")).exit
        yield assertTrue(
          result.isFailure,
          result.causeOption.flatMap(_.failureOption).exists(_.startsWith("Bedrock model is not available")),
        )
    },
    test("records invalid Bedrock response cost and identifies the model") {
      val model = BedrockModelCatalog.Llama4Maverick
      val chooser = new MoveChooser:
        def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] =
          ZIO.fail(MoveChooser.BedrockTurnFailure(
            model.label,
            model.id,
            "returned invalid move JSON: test response",
            inputTokens = 1000,
            outputTokens = 100,
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
    test("limits concurrent games that can spend credentials") {
      val chooser = new MoveChooser:
        def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] = ZIO.never

      withService(chooser):
        for
          service <- ZIO.service[GameService]
          running <- ZIO.foreach(1 to 4)(_ => service.start(StartGameRequest(ModelId, "jev")))
          fifth <- service.start(StartGameRequest(ModelId, "jev")).exit
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
            running <- ZIO.foreach(1 to 4)(_ => service.start(StartGameRequest(ModelId, "jev")))
            _ <- allEntered.await
            cancelling <- service.cancel(running.head.id).fork
            _ <- {
              def awaitCancelled: UIO[Unit] = service.get(running.head.id).flatMap:
                case Some(game) if game.status == GameStatus.Cancelled => ZIO.unit
                case _ => ZIO.yieldNow *> awaitCancelled
              awaitCancelled
            }
            overlappingStart <- service.start(StartGameRequest(ModelId, "jev")).exit
            _ <- release.succeed(())
            _ <- cancelling.join
            _ <- ZIO.foreachDiscard(running.tail)(game => service.cancel(game.id))
          yield overlappingStart
      yield assertTrue(
        result.isFailure,
        result.causeOption.exists(_.failureOption.contains("At most 4 games may run at once")),
      )
    } @@ TestAspect.timeout(5.seconds),
  )
