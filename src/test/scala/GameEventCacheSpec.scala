import ConnectFour.*
import zio.*
import zio.http.*
import zio.json.*
import zio.stream.*
import zio.test.*

object GameEventCacheSpec extends ZIOSpecDefault:
  private val ModelId = BedrockModelCatalog.Glm47Flash.id

  private def withService[A](chooser: MoveChooser)(effect: ZIO[GameService, String, A]): IO[String, A] =
    val dependencies = ZLayer.succeed(chooser) ++ ModelAvailability.allCatalog
    effect.provideLayer(dependencies >>> GameService.layer(Duration.Zero))

  private def withServiceLimit[A](chooser: MoveChooser, limit: Int)(effect: ZIO[GameService, String, A]): IO[String, A] =
    val dependencies = ZLayer.succeed(chooser) ++ ModelAvailability.allCatalog
    effect.provideLayer(dependencies >>> GameService.layer(Duration.Zero, limit))

  private def awaitFinished(service: GameService, id: String): UIO[GameSnapshot] =
    service.get(id).flatMap:
      case Some(game) if game.status != GameStatus.Thinking => ZIO.succeed(game)
      case _ => ZIO.yieldNow *> awaitFinished(service, id)

  private def scriptedChooser(onCall: UIO[Any] = ZIO.unit): MoveChooser = new MoveChooser:
    def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] =
      onCall.as(MoveChooser.Selection(if player == Player.Jev then 0 else 1, "scripted", 10, 2))

  def spec = suite("game events and completed-game cache")(
    test("turn events are ordered, replayable, and independently consumable by multiple users") {
      for
        release <- Promise.make[Nothing, Unit]
        result <- withService(scriptedChooser(release.await)):
          for
            service <- ZIO.service[GameService]
            started <- service.start(StartGameRequest(ModelId, "jev"))
            firstStream <- service.events(started.id).someOrFail("missing first event stream")
            secondStream <- service.events(started.id).someOrFail("missing second event stream")
            firstFiber <- firstStream.runCollect.fork
            secondFiber <- secondStream.runCollect.fork
            _ <- release.succeed(())
            first <- firstFiber.join
            second <- secondFiber.join
          yield (started, first, second)
        (started, first, second) = result
      yield assertTrue(
          first.nonEmpty,
          first == second,
          first.map(_.sequence).toVector == (1L to first.size.toLong).toVector,
          first.map(_.eventType).toVector == Vector.fill(first.size / 2)(Vector(GameEventType.TurnStart, GameEventType.TurnEnd)).flatten,
          first.last.game.status == GameStatus.Won,
          first.forall(_.game.id == started.id),
          first.forall(event => event.toJson.fromJson[GameEvent] == Right(event)),
        )
    } @@ TestAspect.timeout(5.seconds),
    test("a completed model/opening configuration replays without invoking either provider") {
      for
        calls <- Ref.make(0)
        result <- withService(scriptedChooser(calls.update(_ + 1))):
          for
            service <- ZIO.service[GameService]
            computed <- service.start(StartGameRequest(ModelId, "jev"))
            computedStream <- service.events(computed.id).someOrFail("missing computed event stream")
            _ <- computedStream.runDrain
            computedFinished <- awaitFinished(service, computed.id)
            computedCalls <- calls.get
            replay <- service.start(StartGameRequest(ModelId, "jev"))
            replayStream <- service.events(replay.id).someOrFail("missing replay event stream")
            replayEvents <- replayStream.runCollect
            replayFinished <- awaitFinished(service, replay.id)
            replayCalls <- calls.get
          yield (computed, computedFinished, computedCalls, replay, replayEvents, replayFinished, replayCalls)
        (computed, computedFinished, computedCalls, replay, replayEvents, replayFinished, replayCalls) = result
      yield assertTrue(
        !computed.cachedReplay,
        replay.cachedReplay,
        replay.id != computed.id,
        replayEvents.nonEmpty,
        replayEvents.forall(_.game.cachedReplay),
        replayEvents.forall(_.game.id == replay.id),
        replayFinished.moves == computedFinished.moves,
        replayFinished.board == computedFinished.board,
        replayFinished.winner == computedFinished.winner,
        computedCalls == 7,
        replayCalls == computedCalls,
      )
    } @@ TestAspect.timeout(5.seconds),
    test("opening player is part of the cache key") {
      for
        calls <- Ref.make(0)
        result <- withService(scriptedChooser(calls.update(_ + 1))):
          for
            service <- ZIO.service[GameService]
            jevFirst <- service.start(StartGameRequest(ModelId, "jev"))
            _ <- awaitFinished(service, jevFirst.id)
            before <- calls.get
            llmFirst <- service.start(StartGameRequest(ModelId, "llm"))
            _ <- awaitFinished(service, llmFirst.id)
            after <- calls.get
          yield (llmFirst, before, after)
        (llmFirst, before, after) = result
      yield assertTrue(!llmFirst.cachedReplay, after > before)
    } @@ TestAspect.timeout(5.seconds),
    test("SSE frames carry named turn events with raw GameEvent JSON") {
      withService(scriptedChooser()):
        for
          service <- ZIO.service[GameService]
          started <- service.start(StartGameRequest(ModelId, "jev"))
          stream <- service.events(started.id).someOrFail("missing event stream")
          event <- stream.runHead.someOrFail("missing first event")
          response = Response.fromServerSentEvents(ZStream.succeed(ServerSentEvent(
            data = event.toJson,
            eventType = Some(event.eventType.sseName),
            id = Some(event.sequence.toString),
          )))
          encoded <- response.body.asString.orDie
        yield assertTrue(
          encoded.contains("event: turn-start"),
          encoded.contains(s"id: ${event.sequence}"),
          encoded.contains(s"data: ${event.toJson}"),
          !encoded.contains("data: \""),
        )
    } @@ TestAspect.timeout(5.seconds),
    test("event streams resume strictly after the supplied sequence") {
      withService(scriptedChooser()):
        for
          service <- ZIO.service[GameService]
          started <- service.start(StartGameRequest(ModelId, "jev"))
          fullStream <- service.events(started.id).someOrFail("missing full stream")
          full <- fullStream.runCollect
          after = full(full.size / 2).sequence
          resumedStream <- service.events(started.id, after).someOrFail("missing resumed stream")
          resumed <- resumedStream.runCollect
          completedStream <- service.events(started.id, full.last.sequence).someOrFail("missing completed stream")
          completed <- completedStream.runCollect
        yield assertTrue(
          resumed == full.filter(_.sequence > after),
          resumed.forall(_.sequence > after),
          completed.isEmpty,
          !Main.terminalEventAcknowledged(full.last.game, Some(full.last.sequence - 1L), full.last),
          Main.terminalEventAcknowledged(full.last.game, Some(full.last.sequence), full.last),
          !Main.terminalEventAcknowledged(full.last.game, None, full.last),
        )
    } @@ TestAspect.timeout(5.seconds),
    test("cancellation publishes one terminal event and is idempotent") {
      withService(new MoveChooser:
        def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] = ZIO.never
      ):
        for
          service <- ZIO.service[GameService]
          started <- service.start(StartGameRequest(ModelId, "jev"))
          first <- service.cancel(started.id)
          second <- service.cancel(started.id)
          stream <- service.events(started.id).someOrFail("missing cancelled stream")
          events <- stream.runCollect
        yield assertTrue(
          first.exists(_.status == GameStatus.Cancelled),
          second == first,
          events.count(_.eventType == GameEventType.TurnEnd) == 1,
          events.last.game.status == GameStatus.Cancelled,
        )
    } @@ TestAspect.timeout(5.seconds),
    test("all active sessions are bounded independently of paid concurrency") {
      val chooser = new MoveChooser:
        def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection] = ZIO.never
      withServiceLimit(chooser, 2):
        for
          service <- ZIO.service[GameService]
          first <- service.start(StartGameRequest(ModelId, "jev"))
          second <- service.start(StartGameRequest(ModelId, "llm"))
          third <- service.start(StartGameRequest(BedrockModelCatalog.Qwen332B.id, "jev")).exit
          _ <- service.cancel(first.id)
          _ <- service.cancel(second.id)
        yield assertTrue(
          third.isFailure,
          third.causeOption.exists(_.failureOption.contains("At most 2 active game sessions may run at once")),
        )
    } @@ TestAspect.timeout(5.seconds),
    test("cached replay timing uses move duration and the configured inter-turn pause") {
      withService(scriptedChooser()):
        for
          service <- ZIO.service[GameService]
          started <- service.start(StartGameRequest(ModelId, "jev"))
          stream <- service.events(started.id).someOrFail("missing event stream")
          events <- stream.runCollect
          firstStart = events.head
          laterStart = events.find(event => event.eventType == GameEventType.TurnStart && event.sequence > 1).get
          firstEnd = events.find(_.eventType == GameEventType.TurnEnd).get
          pause = 650.millis
        yield assertTrue(
          GameService.replayDelay(firstStart, 0, pause) == Duration.Zero,
          GameService.replayDelay(laterStart, 2, pause) == pause,
          GameService.replayDelay(firstEnd, 1, pause) == Duration.fromMillis(firstEnd.game.moves.last.durationMs),
        )
    } @@ TestAspect.timeout(5.seconds),
  )
