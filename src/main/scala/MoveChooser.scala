import com.jamesward.zio_bedrock_converse.Bedrock
import com.jamesward.zio_typesafe_ai.TypeSafeAI
import ConnectFour.*
import zio.*
import zio.http.{Client as HttpClient}
import zio.json.*

trait MoveChooser:
  def choose(player: Player, board: Board, modelId: String): Task[MoveChooser.Selection]

object MoveChooser:
  final case class Selection(
    column: Int,
    note: String,
    inputTokens: Int = 0,
    outputTokens: Int = 0,
  )

  final case class BedrockTurnFailure(
    modelLabel: String,
    modelId: String,
    details: String,
    inputTokens: Int = 0,
    outputTokens: Int = 0,
  ) extends RuntimeException(s"$modelLabel ($modelId): $details")

  private final case class BedrockMove(column: Int) derives JsonDecoder

  val live: URLayer[HttpClient, MoveChooser] =
    ZLayer.fromFunction(Live.apply)

  private final case class Live(httpClient: HttpClient) extends MoveChooser:
    def choose(player: Player, board: Board, modelId: String): Task[Selection] =
      player match
        case Player.Jev => chooseWithJev(board)
        case Player.Llm =>
          ZIO.fromOption(BedrockModelCatalog.find(modelId))
            .orElseFail(IllegalArgumentException(s"Unsupported Bedrock model: $modelId"))
            .flatMap(chooseWithBedrock(board, _))

    private def env(name: String): Task[String] =
      ZIO.systemWith(_.env(name)).orDie.flatMap:
        case Some(value) if value.trim.nonEmpty => ZIO.succeed(value.trim)
        case _ => ZIO.fail(IllegalStateException(s"Missing $name; set it before starting a game"))

    private def chooseWithJev(board: Board): Task[Selection] =
      for
        apiKey <- env("TYPESAFE_API_KEY")
        valid = board.validColumns
        criteria <- ZIO.fromEither(TypeSafeAI.ChoiceCriteria.fromContent(
          valid.map: column =>
            s"column_$column" -> (TypeSafeAI.Content(columnDescription(board, Player.Jev, column)): TypeSafeAI.Content | Null)
          .toMap
        )).mapError(IllegalArgumentException(_))
        state =
          s"""You are Jev playing Connect Four as J against L. It is your turn.
             |Rules: columns are numbered 0 through 6. A piece falls to the lowest empty row in its column.
             |Rows are numbered 0 through 5 from top to bottom. A full column cannot be played.
             |The first player with four adjacent pieces horizontally, vertically, or diagonally wins.
             |The board below is the complete current game state: J is Jev, L is the opponent, and . is empty.
             |Only the supplied Choice options are legal columns.
             |
             |${board.promptView}""".stripMargin
        request <- ZIO.fromEither(TypeSafeAI.askDynamic(
          state,
          List(
            TypeSafeAI.QuestionId("move") -> TypeSafeAI.Question.Choice(
              "Choose exactly one legal column. Prefer an immediate win, then block an immediate loss, then build connected threats near the center.",
              criteria,
            )
          ),
        )).mapError(IllegalArgumentException(_))
        result <- request.run.provideLayer(
          ZLayer.succeed(httpClient) >>> TypeSafeAI.Client.layer(TypeSafeAI.ApiKey(apiKey))
        )
        answer <- ZIO.fromOption(result.answers.get(TypeSafeAI.QuestionId("move")))
          .orElseFail(IllegalStateException("Jev did not answer the move question"))
        choice <- answer match
          case TypeSafeAI.DynamicAnswer.Choice(value) => ZIO.succeed(value)
          case other => ZIO.fail(IllegalStateException(s"Jev returned an unexpected answer: $other"))
        column <- ZIO.fromOption(choice.choice.stripPrefix("column_").toIntOption)
          .orElseFail(IllegalStateException(s"Jev selected an invalid option: ${choice.choice}"))
        _ <- ZIO.fail(IllegalStateException(s"Jev selected illegal column $column")).unless(valid.contains(column))
      yield Selection(
        column,
        f"Jev confidence ${(choice.confidence.unwrap * 100)}%.0f%%",
        result.usage.inputTokens,
        result.usage.outputTokens,
      )

    private def chooseWithBedrock(
      board: Board,
      model: BedrockModelCatalog.Model,
    ): Task[Selection] =
      for
        apiKey <- env("AWS_BEARER_TOKEN_BEDROCK")
        valid = board.validColumns
        legalLandings = valid.flatMap: column =>
          board.play(column, Player.Llm).toOption.map:
            case (_, row) => s"column $column lands at row $row"
        fullColumns = (0 until ConnectFour.Columns).filterNot(valid.contains)
        prompt =
          s"""You are the LLM player (L) in Connect Four against Jev (J). It is your turn.
             |Rules:
             |- Columns are numbered 0 through 6.
             |- A played piece falls to the lowest empty row in its column; you cannot choose a full column.
             |- Rows are numbered 0 through 5 from top to bottom.
             |- The first player with four adjacent pieces horizontally, vertically, or diagonally wins.
             |- J is Jev, L is you, and . is an empty cell.
             |
             |The board below is the complete current game state, followed by column numbers:
             |${board.promptView}
             |
             |Your legal options are exactly: ${valid.mkString(", ")}.
             |Legal landing cells: ${legalLandings.mkString("; ")}.
             |Full columns that must not be selected: ${if fullColumns.isEmpty then "none" else fullColumns.mkString(", ")}.
             |Choose one of the legal options. Prefer an immediate win, then block an immediate Jev win, then create threats near the center.
             |Your only decision is the column. Respond with only one JSON object in this exact shape: {"column":3}
             |Do not add any other fields, prose, or markdown fences.""".stripMargin
        result <- Bedrock.converse(Bedrock.RequestConfig(
          messages = List(Bedrock.Message.user(prompt)),
          system = "Play Connect Four accurately and return only the requested JSON object.",
          inferenceConfig = Bedrock.InferenceConfig(maxTokens = model.maxOutputTokens),
        )).asResponse.provideLayer(
          ZLayer.succeed(httpClient) >>> Bedrock.Client.layer(
            Bedrock.ApiKey(apiKey),
            Bedrock.Region.UsEast1,
            Bedrock.ModelId(model.id),
          )
        ).mapError(error => BedrockTurnFailure(
          model.label,
          model.id,
          s"Bedrock turn failed: ${error.getMessage}",
        ))
        selection <- decodeMove(result.output.text) match
          case Right(move) if valid.contains(move.column) =>
            ZIO.succeed(Selection(
              move.column,
              s"${model.label} selected column ${move.column}",
              result.usage.inputTokens,
              result.usage.outputTokens,
            ))
          case Right(move) => ZIO.fail(BedrockTurnFailure(
            model.label,
            model.id,
            s"selected illegal column ${move.column}; legal columns: ${valid.mkString(", ")}; " +
              s"response: ${result.output.text.take(500)}",
            result.usage.inputTokens,
            result.usage.outputTokens,
          ))
          case Left(error) => ZIO.fail(BedrockTurnFailure(
            model.label,
            model.id,
            s"returned invalid move JSON: $error; response: ${result.output.text.take(500)}",
            result.usage.inputTokens,
            result.usage.outputTokens,
          ))
      yield selection

    private def decodeMove(text: String): Either[String, BedrockMove] =
      val start = text.indexOf('{')
      val end = text.lastIndexOf('}')
      if start < 0 || end < start then Left("response did not contain a JSON object")
      else text.substring(start, end + 1).fromJson[BedrockMove]

    private def columnDescription(board: Board, player: Player, column: Int): String =
      board.play(column, player).toOption match
        case Some((next, _)) if next.winner.contains(player) => s"Column $column: an immediate winning move"
        case _ if isImmediateWin(board, player.opponent, column) => s"Column $column: blocks an opponent winning lane"
        case _ => s"Column $column: legal move, distance ${math.abs(3 - column)} from center"

    private def isImmediateWin(board: Board, player: Player, column: Int): Boolean =
      board.play(column, player).toOption.exists(_._1.winner.contains(player))
