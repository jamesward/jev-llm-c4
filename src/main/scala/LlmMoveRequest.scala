import ConnectFour.*
import zio.json.*

object LlmMoveRequest:
  final case class Details(
    modelId: String,
    backend: String,
    request: MoveRequest.Data,
  ) derives JsonCodec:
    def message: String = request.toString

  def from(board: Board, model: BedrockModelCatalog.Model): Details =
    Details(
      modelId = model.id,
      backend = model.backend.label,
      request = MoveRequest.from(board, Player.Llm),
    )
