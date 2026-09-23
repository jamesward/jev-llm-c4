import ConnectFour.*
import zio.json.*
import zio.test.*

object LlmMoveRequestSpec extends ZIOSpecDefault:
  def spec = suite("simple shared LLM move request")(
    test("renders the compact board, minimal rules, and playable choices") {
      val details = LlmMoveRequest.from(Board.empty, BedrockModelCatalog.Glm47Flash)
      assertTrue(
        details.request == MoveRequest.from(Board.empty, Player.Llm),
        details.message == details.request.toString,
        details.message.contains(MoveRequest.instructions.question),
        MoveRequest.instructions.rules.forall(details.message.contains),
        MoveRequest.instructions.goalsInOrder.forall(details.message.contains),
        details.message.contains("Board rows top to bottom:"),
        details.message.contains("......."),
        details.message.contains("Columns: 0 1 2 3 4 5 6"),
        details.message.contains("Choices: column_0, column_1, column_2"),
        details.message.length < 600,
        details.toJson.fromJson[LlmMoveRequest.Details] == Right(details),
      )
    },
    test("logs the exact LLM message without credentials") {
      val model = BedrockModelCatalog.Glm47Flash
      val details = LlmMoveRequest.from(Board.empty, model)
      val log = MoveChooser.llmRequestLog(model, details)
      assertTrue(
        log.contains(s"model=${model.id}"),
        log.contains(s"backend=${model.backend.label}"),
        log.endsWith(s"message:\n${details.message}"),
        !log.contains("AWS_BEARER_TOKEN_BEDROCK"),
        !log.toLowerCase.contains("apikey"),
      )
    },
    test("uses the same board, instructions, and choices as Jev") {
      val jev = MoveRequest.from(Board.empty, Player.Jev)
      val llm = MoveRequest.from(Board.empty, Player.Llm)
      assertTrue(
        llm.instructions == jev.instructions,
        llm.state.boardRowsTopToBottom == jev.state.boardRowsTopToBottom,
        llm.choices == jev.choices,
        llm.state.currentPlayerPiece == jev.state.opponentPiece,
        llm.state.opponentPiece == jev.state.currentPlayerPiece,
      )
    },
    test("does not include tactical evidence or output-schema instructions") {
      val message = LlmMoveRequest.from(Board.empty, BedrockModelCatalog.Glm47Flash).message.toLowerCase
      val forbidden = Vector(
        "postdrop",
        "candidate",
        "diagonalwindow",
        "json object",
        "integer field",
        "response format",
        "winning column",
      )
      assertTrue(forbidden.forall(term => !message.contains(term)))
    },
  )
