import ConnectFour.*
import zio.*
import zio.http.Client
import zio.test.*
import zio.test.TestAspect.*

object BedrockModelIntegrationSpec extends ZIOSpecDefault:
  private val requestedIds =
    sys.env.get("BEDROCK_INTEGRATION_MODEL_IDS")
      .map(_.split(',').iterator.map(_.trim).filter(_.nonEmpty).toSet)

  private val models =
    requestedIds.fold(BedrockModelCatalog.all)(ids => BedrockModelCatalog.all.filter(model => ids.contains(model.id)))

  private val rejectedRequestedIds =
    requestedIds.toVector.flatMap(_ -- models.map(_.id))

  def spec =
    val requestedModelsAreSelectable =
      test("every explicitly requested model supports native structured output") {
        assertTrue(rejectedRequestedIds.isEmpty)
      }
    val modelSpecs = models.map: model =>
      test(s"${model.label} via ${model.backend.label} is hidden or returns one legal turn with usage") {
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
                  selection.requestDetails.contains("Board rows top to bottom:"),
                  selection.requestDetails.contains("Choices: column_0"),
                  selection.requestDetails.contains("Four adjacent pieces"),
                  !selection.requestDetails.contains("PostDropCandidate"),
                  selection.responseDetails.nonEmpty,
                )
            case None =>
              val reason = availability.snapshot.hidden.find(_.model.id == model.id).map(_.reason).getOrElse("unknown")
              requestedIds match
                case Some(_) => ZIO.fail(RuntimeException(s"Requested integration model ${model.label} is unavailable: $reason"))
                case None =>
                  ZIO.log(s"Skipping unavailable integration model ${model.label}: $reason") *>
                    ZIO.succeed(assertTrue(reason.nonEmpty))
        yield result
      }
    suite("fixed Bedrock model turn integration")(requestedModelsAreSelectable +: modelSpecs).provideShared(
      Client.default,
      ModelAvailability.live,
      MoveChooser.live,
    ) @@ ifEnvSet("AWS_BEARER_TOKEN_BEDROCK") @@ withLiveSystem @@ withLiveClock @@ timeout(4.minutes) @@ sequential
