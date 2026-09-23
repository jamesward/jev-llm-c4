import ConnectFour.*
import zio.json.*
import zio.schema.{Schema, derived}

object MoveRequest:
  final case class State(
    currentPlayerPiece: String,
    opponentPiece: String,
    emptyCell: String,
    boardRowsTopToBottom: Vector[String],
  ) derives Schema, JsonCodec

  final case class Instructions(
    question: String,
    rules: Vector[String],
    goalsInOrder: Vector[String],
  ) derives Schema, JsonCodec

  final case class Data(
    state: State,
    instructions: Instructions,
    choices: Vector[String],
  ) derives Schema, JsonCodec:
    override def toString: String =
      s"""${instructions.question}
         |Current: ${state.currentPlayerPiece}; opponent: ${state.opponentPiece}; empty: ${state.emptyCell}
         |Board rows top to bottom:
         |${state.boardRowsTopToBottom.mkString("\n")}
         |Columns: 0 1 2 3 4 5 6
         |${instructions.rules.mkString(" ")}
         |${instructions.goalsInOrder.mkString(" ")}
         |Choices: ${choices.mkString(", ")}""".stripMargin

  val instructions: Instructions = Instructions(
    question = "Which legal column should `currentPlayerPiece` play?",
    rules = Vector(
      "Each piece falls to the lowest empty cell in its column.",
      "Four adjacent pieces horizontally, vertically, or diagonally wins.",
    ),
    goalsInOrder = Vector("Choose the best legal column."),
  )

  def from(board: Board, player: Player): Data =
    val (currentPlayerPiece, opponentPiece) = player match
      case Player.Jev => "J" -> "L"
      case Player.Llm => "L" -> "J"
    Data(
      state = State(
        currentPlayerPiece = currentPlayerPiece,
        opponentPiece = opponentPiece,
        emptyCell = ".",
        boardRowsTopToBottom = board.symbolRows.map(_.mkString),
      ),
      instructions = instructions,
      choices = board.validColumns.map(column => s"column_$column"),
    )
