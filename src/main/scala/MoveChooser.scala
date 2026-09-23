import com.jamesward.zio_bedrock.{Bedrock, Converse, ConverseConfig, Mantle, MantleConfig}
import com.jamesward.zio_bedrock.Bedrock.{ApiKey, InferenceConfig, Message, ModelId, Region, RequestConfig}
import com.jamesward.zio_typesafe_ai.TypeSafeAI
import ConnectFour.*
import zio.*
import zio.http.{Client as HttpClient, URL}
import zio.json.*
import zio.schema.{Schema, derived}

trait MoveChooser:
  def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection]

object MoveChooser:
  final case class Selection(
    column: Int,
    note: String,
    inputTokens: Int = 0,
    outputTokens: Int = 0,
    requestDetails: String = "",
    responseDetails: String = "",
  )

  sealed abstract class TurnFailure(message: String) extends RuntimeException(message):
    def attemptedColumn: Option[Int]
    def inputTokens: Int
    def outputTokens: Int
    def requestDetails: String
    def responseDetails: String

  final case class JevTurnFailure(
    details: String,
    attemptedColumn: Option[Int] = None,
    inputTokens: Int = 0,
    outputTokens: Int = 0,
    requestDetails: String = "",
    responseDetails: String = "",
  ) extends TurnFailure(details)

  final case class BedrockTurnFailure(
    modelLabel: String,
    modelId: String,
    details: String,
    attemptedColumn: Option[Int] = None,
    inputTokens: Int = 0,
    outputTokens: Int = 0,
    requestDetails: String = "",
    responseDetails: String = "",
  ) extends TurnFailure(s"$modelLabel ($modelId): $details")

  def jevRequestLog(request: JevMoveRequest.RequestDetails): String =
    s"Jev request question=${JevMoveRequest.QuestionId} payload=${request.toJson}"

  def llmRequestLog(model: BedrockModelCatalog.Model, details: LlmMoveRequest.Details): String =
    s"LLM request model=${model.id} backend=${model.backend.label} " +
      s"maxOutputTokens=${model.maxOutputTokens}\nmessage:\n${details.message}"

  private final case class BedrockMove(column: Int) derives Schema, JsonCodec

  val live: URLayer[HttpClient, MoveChooser] =
    ZLayer.fromFunction(Live.apply)

  private final case class Live(httpClient: HttpClient) extends MoveChooser:
    def choose(player: Player, board: Board, modelId: String): Task[Selection] =
      player match
        case Player.Jev => chooseWithJev(board)
        case Player.Llm =>
          ZIO.fromOption(BedrockModelCatalog.find(modelId))
            .orElseFail(IllegalArgumentException(s"Unsupported LLM model: $modelId"))
            .flatMap(chooseWithBedrock(board, _))

    private def env(name: String): Task[String] =
      ZIO.systemWith(_.env(name)).orDie.flatMap:
        case Some(value) if value.trim.nonEmpty => ZIO.succeed(value.trim)
        case _ => ZIO.fail(IllegalStateException(s"Missing $name; set it before starting a game"))

    private def chooseWithJev(board: Board): Task[Selection] =
      for
        apiKey <- env("TYPESAFE_API_KEY")
        data = MoveRequest.from(board, Player.Jev)
        valid = board.validColumns
        requestDetails = JevMoveRequest.requestDetails(data)
        _ <- ZIO.logInfo(jevRequestLog(requestDetails))
        criteria <- ZIO.fromEither(JevMoveRequest.criteria(data)).mapError(IllegalArgumentException(_))
        result <- TypeSafeAI.ask(
          data.state,
          (move = TypeSafeAI.Question.Choice(data.instructions, criteria)),
        ).run.provideLayer(
          ZLayer.succeed(httpClient) >>> TypeSafeAI.Client.layer(TypeSafeAI.ApiKey(apiKey))
        ).tapError: error =>
          ZIO.logError(s"Jev request failed question=${JevMoveRequest.QuestionId}: ${error.getMessage}")
        .mapError(error => JevTurnFailure(
          s"Jev request failed: ${error.getMessage}",
          requestDetails = requestDetails.toJson,
        ))
        choice = result.answers.move
        responseDetails = JevMoveRequest.responseDetails(choice).toJson
        _ <- ZIO.logInfo(
          s"Jev response model=${result.model.unwrap} inputTokens=${result.usage.inputTokens} " +
            s"outputTokens=${result.usage.outputTokens} details=$responseDetails"
        )
        column <- ZIO.fromOption(choice.choice.stripPrefix("column_").toIntOption)
          .orElseFail(JevTurnFailure(
            s"Jev selected an invalid option: ${choice.choice}",
            inputTokens = result.usage.inputTokens,
            outputTokens = result.usage.outputTokens,
            requestDetails = requestDetails.toJson,
            responseDetails = responseDetails,
          ))
        _ <- ZIO.fail(JevTurnFailure(
          s"Jev selected illegal column $column",
          attemptedColumn = Some(column),
          inputTokens = result.usage.inputTokens,
          outputTokens = result.usage.outputTokens,
          requestDetails = requestDetails.toJson,
          responseDetails = responseDetails,
        )).unless(valid.contains(column))
      yield Selection(
        column,
        f"Jev confidence ${(choice.confidence.unwrap * 100)}%.0f%%",
        result.usage.inputTokens,
        result.usage.outputTokens,
        requestDetails.toJson,
        responseDetails,
      )

    private def chooseWithBedrock(
      board: Board,
      model: BedrockModelCatalog.Model,
    ): Task[Selection] =
      for
        apiKey <- env("AWS_BEARER_TOKEN_BEDROCK")
        valid = board.validColumns
        details = LlmMoveRequest.from(board, model)
        _ <- ZIO.logInfo(llmRequestLog(model, details))
        llmRequest = Bedrock.chat(RequestConfig(
          messages = List(Message.user(details.message)),
          inferenceConfig = InferenceConfig(maxTokens = model.maxOutputTokens),
        )).asResponse[BedrockMove].provideLayer(bedrockLayer(httpClient, apiKey, model))
        result <- llmRequest
          .tapError(error => ZIO.logError(
            s"LLM request failed model=${model.id} backend=${model.backend.label}: ${error.getMessage}"
          ))
          .mapError(error => BedrockTurnFailure(
            model.label,
            model.id,
            s"LLM turn failed: ${error.getMessage.replaceAll("(?i)bedrock", "LLM provider")}",
            requestDetails = details.message,
          ))
        _ <- ZIO.logInfo(
          s"LLM response model=${model.id} backend=${model.backend.label} stopReason=${result.stopReason} " +
            s"inputTokens=${result.usage.inputTokens} outputTokens=${result.usage.outputTokens} " +
            s"structured=${result.output.toJson}"
        )
        move = result.output
        selection <-
          if valid.contains(move.column) then
            ZIO.succeed(Selection(
              move.column,
              s"${model.label} selected column ${move.column}",
              result.usage.inputTokens,
              result.usage.outputTokens,
              details.message,
              move.toJson,
            ))
          else ZIO.fail(BedrockTurnFailure(
            model.label,
            model.id,
            s"selected illegal column ${move.column}; legal columns: ${valid.mkString(", ")}; " +
              s"response: ${move.toJson}",
            attemptedColumn = Some(move.column),
            inputTokens = result.usage.inputTokens,
            outputTokens = result.usage.outputTokens,
            requestDetails = details.message,
            responseDetails = move.toJson,
          ))
      yield selection

    private def bedrockLayer(
      httpClient: HttpClient,
      apiKey: String,
      model: BedrockModelCatalog.Model,
    ): ULayer[Bedrock] =
      val client = ZLayer.succeed(httpClient)
      model.backend match
        case BedrockModelCatalog.Backend.Converse =>
          client >>> Converse.layer(ConverseConfig(
            ApiKey(apiKey),
            Region.UsEast1,
            ModelId(model.id),
          ))
        case BedrockModelCatalog.Backend.Mantle =>
          val config = model.mantleEndpoint match
            case BedrockModelCatalog.MantleEndpoint.Standard =>
              MantleConfig(
                ApiKey(apiKey),
                Region.UsEast1,
                ModelId(model.id),
              )
            case BedrockModelCatalog.MantleEndpoint.OpenAI =>
              val endpoint = URL.decode(
                s"https://bedrock-mantle.${BedrockModelCatalog.RegionCode}.api.aws/openai"
              ).toOption.get
              MantleConfig(ApiKey(apiKey), endpoint, ModelId(model.id))
          client >>> Mantle.layer(config)
