import com.jamesward.zio_typesafe_ai.TypeSafeAI
import zio.json.*
import zio.json.ast.Json

import scala.collection.immutable.VectorMap

object JevMoveRequest:
  val QuestionId = "move"

  final case class ChoiceQuestion(
    `type`: String,
    instructions: MoveRequest.Instructions,
    criteria: Map[String, Json],
  ) derives JsonCodec

  final case class RequestDetails(
    state: MoveRequest.State,
    model: String,
    questions: Map[String, ChoiceQuestion],
  ) derives JsonCodec

  final case class ResponseDetails(
    choice: String,
    confidence: Double,
  ) derives JsonCodec

  def requestDetails(data: MoveRequest.Data): RequestDetails =
    RequestDetails(
      state = data.state,
      model = TypeSafeAI.ModelId.JevLatest.unwrap,
      questions = Map(
        QuestionId -> ChoiceQuestion(
          `type` = "choice",
          instructions = data.instructions,
          criteria = VectorMap.from(data.choices.map(_ -> Json.Null)),
        )
      ),
    )

  def criteria(data: MoveRequest.Data): Either[String, TypeSafeAI.ChoiceCriteria] =
    TypeSafeAI.ChoiceCriteria.fromContent(VectorMap.from(
      data.choices.map(choice => choice -> (null: TypeSafeAI.Content | Null))
    ))

  def responseDetails(answer: TypeSafeAI.ChoiceAnswer): ResponseDetails =
    ResponseDetails(answer.choice, answer.confidence.unwrap)
