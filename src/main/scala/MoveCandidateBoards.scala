import ConnectFour.*

object MoveCandidateBoards:
  final case class Candidate(
    choiceId: String,
    column: Int,
    description: String,
  )

  def from(board: Board, player: Player): Vector[Candidate] =
    candidates(board, player, includeOpponentReplies = false)

  def withOpponentReplies(board: Board, player: Player): Vector[Candidate] =
    candidates(board, player, includeOpponentReplies = true)

  private def candidates(
    board: Board,
    player: Player,
    includeOpponentReplies: Boolean,
  ): Vector[Candidate] =
    Vector.tabulate(ConnectFour.Columns): column =>
      val choiceId = s"column_$column"
      val description = board.play(column, player) match
        case Right((resultingBoard, _)) =>
          val candidateBoard =
            s"""After ${piece(player)} is dropped into column $column, the board is:
               |${ascii(resultingBoard)}""".stripMargin
          if includeOpponentReplies then
            candidateBoard + "\n\n" + opponentReplies(resultingBoard, player.opponent)
          else candidateBoard
        case Left(error) =>
          s"Column $column cannot be taken: $error"
      Candidate(choiceId, column, description)

  private def opponentReplies(board: Board, opponent: Player): String =
    val replies = Vector.tabulate(ConnectFour.Columns): column =>
      board.play(column, opponent) match
        case Right((resultingBoard, _)) =>
          s"""After ${piece(opponent)} replies in column $column, the board is:
             |${ascii(resultingBoard)}""".stripMargin
        case Left(error) =>
          s"Reply column $column cannot be taken: $error"
    s"Possible ${piece(opponent)} replies after this candidate:\n${replies.mkString("\n\n")}"

  private def piece(player: Player): String = player match
    case Player.Jev => "J"
    case Player.Llm => "L"

  private def ascii(board: Board): String =
    (board.symbolRows.map(_.mkString(" ")) :+ "0 1 2 3 4 5 6").mkString("\n")
