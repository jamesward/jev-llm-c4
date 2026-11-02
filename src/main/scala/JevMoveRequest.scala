import com.jamesward.zio_decisions.Decisions
import com.jamesward.zio_decisions.typesafe.TypeSafe
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
      model = TypeSafe.JevLatest.unwrap,
      questions = Map(
        QuestionId -> ChoiceQuestion(
          `type` = "choice",
          instructions = data.instructions,
          criteria = VectorMap.from(data.choices.map(_ -> Json.Null)),
        )
      ),
    )

  def criteria(data: MoveRequest.Data): Either[String, Decisions.ChoiceCriteria] =
    Decisions.ChoiceCriteria.fromContent(VectorMap.from(
      data.choices.map(choice => choice -> (null: Decisions.Content | Null))
    ))

  def responseDetails(answer: Decisions.ChoiceAnswer): ResponseDetails =
    ResponseDetails(answer.choice, answer.confidence.unwrap)
