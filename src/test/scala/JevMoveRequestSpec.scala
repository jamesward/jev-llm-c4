import ConnectFour.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object JevMoveRequestSpec extends ZIOSpecDefault:
  private def playAll(moves: (Int, Player)*): Board =
    moves.foldLeft(Board.empty):
      case (board, (column, player)) => board.play(column, player).toOption.get._1

  private val fullColumn = playAll(
    0 -> Player.Jev,
    0 -> Player.Llm,
    0 -> Player.Jev,
    0 -> Player.Llm,
    0 -> Player.Jev,
    0 -> Player.Llm,
  )

  def spec = suite("simple Jev move request")(
    test("builds six compact rows and only playable column choices") {
      val data = MoveRequest.from(fullColumn, Player.Jev)
      assertTrue(
        data.state.currentPlayerPiece == "J",
        data.state.opponentPiece == "L",
        data.state.emptyCell == ".",
        data.state.boardRowsTopToBottom == fullColumn.symbolRows.map(_.mkString),
        data.state.boardRowsTopToBottom.size == ConnectFour.Rows,
        data.state.boardRowsTopToBottom.forall(_.length == ConnectFour.Columns),
        !data.choices.contains("column_0"),
        data.choices == fullColumn.validColumns.map(column => s"column_$column"),
        data.instructions == MoveRequest.instructions,
        data.instructions.rules.size == 2,
        data.instructions.goalsInOrder == Vector("Choose the best legal column."),
        data.toJson.fromJson[MoveRequest.Data] == Right(data),
      )
    },
    test("matches the one-question TypeSafe Choice envelope") {
      val data = MoveRequest.from(fullColumn, Player.Jev)
      val details = JevMoveRequest.requestDetails(data)
      val question = details.questions(JevMoveRequest.QuestionId)
      val root = details.toJson.fromJson[Json].toOption.flatMap(_.asObject).get
      assertTrue(
        root.fields.map(_._1).toSet == Set("state", "model", "questions"),
        details.state == data.state,
        details.model == "jev-latest",
        details.questions.keySet == Set("move"),
        question.`type` == "choice",
        question.instructions == data.instructions,
        question.criteria.keys.toVector == data.choices,
        question.criteria.values.forall(_ == Json.Null),
        JevMoveRequest.criteria(data).toOption.exists(_.options.keys.toVector == data.choices),
        !details.toJson.contains("column_0"),
        details.toJson.fromJson[JevMoveRequest.RequestDetails] == Right(details),
      )
    },
    test("logs the exact simple request without credentials") {
      val details = JevMoveRequest.requestDetails(MoveRequest.from(Board.empty, Player.Jev))
      val log = MoveChooser.jevRequestLog(details)
      assertTrue(
        log == s"Jev request question=move payload=${details.toJson}",
        !log.contains("TYPESAFE_API_KEY"),
        !log.toLowerCase.contains("apikey"),
      )
    },
    test("contains no candidate boards, windows, or tactical conclusions") {
      val json = JevMoveRequest.requestDetails(MoveRequest.from(Board.empty, Player.Jev)).toJson
      assertTrue(
        !json.contains("PostDrop"),
        !json.contains("Candidate"),
        !json.contains("DiagonalWindow"),
        !json.contains("winningColumn"),
        !json.contains("legalColumns"),
      )
    },
    test("response details retain only choice and confidence") {
      val details = JevMoveRequest.ResponseDetails("column_3", 0.8)
      val json = details.toJson
      assertTrue(
        json.fromJson[JevMoveRequest.ResponseDetails] == Right(details),
        json.contains("column_3"),
        !json.contains("probabilities"),
        !json.contains("route"),
      )
    },
  )
