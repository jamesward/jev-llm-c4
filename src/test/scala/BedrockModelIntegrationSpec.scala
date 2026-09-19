import ConnectFour.*
import zio.*
import zio.http.Client
import zio.test.*
import zio.test.TestAspect.*

object BedrockModelIntegrationSpec extends ZIOSpecDefault:
  def spec = suite("fixed Bedrock model turn integration")(
    BedrockModelCatalog.all.map: model =>
      test(s"${model.label} is hidden or returns one legal turn with usage") {
        for
          availability <- ZIO.service[ModelAvailability]
          result <- availability.findAvailable(model.id) match
            case Some(_) =>
              ZIO.serviceWithZIO[MoveChooser](_.choose(Player.Llm, Board.empty, model.id)).map: selection =>
                assertTrue(
                  Board.empty.validColumns.contains(selection.column),
                  selection.note.nonEmpty,
                  selection.inputTokens > 0,
                  selection.outputTokens > 0,
                )
            case None =>
              val hidden = availability.snapshot.hidden.find(_.model.id == model.id)
              ZIO.log(s"Skipping unavailable integration model ${model.label}: ${hidden.map(_.reason).getOrElse("unknown")}") *>
                ZIO.succeed(assertTrue(hidden.exists(_.reason.nonEmpty)))
        yield result
      }
  ).provideShared(
    Client.default,
    ModelAvailability.live,
    MoveChooser.live,
  ) @@ ifEnvSet("AWS_BEARER_TOKEN_BEDROCK") @@ withLiveSystem @@ withLiveClock @@ timeout(4.minutes) @@ sequential
