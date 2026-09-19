import zio.json.*

object ConnectFour:
  val Columns = 7
  val Rows = 6

  enum Player derives JsonCodec:
    case Jev, Llm

    def opponent: Player = this match
      case Jev => Llm
      case Llm => Jev

    def label: String = this match
      case Jev => "Jev"
      case Llm => "LLM"

  final case class TacticalOption(
    column: Int,
    landingRow: Int,
    winsNow: Boolean,
    blocksImmediateThreat: Boolean,
    opponentWinningReplies: Vector[Int],
    ownWinningThreats: Vector[Int],
    centerDistance: Int,
  )

  final case class Board private (cells: Vector[Vector[Option[Player]]]):
    def validColumns: Vector[Int] =
      (0 until Columns).filter(column => cells.head(column).isEmpty).toVector

    def play(column: Int, player: Player): Either[String, (Board, Int)] =
      if column < 0 || column >= Columns then Left(s"Column must be between 0 and ${Columns - 1}")
      else
        (Rows - 1 to 0 by -1).find(row => cells(row)(column).isEmpty) match
          case None => Left(s"Column $column is full")
          case Some(row) =>
            val nextRow = cells(row).updated(column, Some(player))
            Right(Board(cells.updated(row, nextRow)) -> row)

    def winningColumns(player: Player): Vector[Int] =
      validColumns.filter: column =>
        play(column, player).toOption.exists(_._1.winner.contains(player))

    def tacticalOptions(player: Player): Vector[TacticalOption] =
      val opponentThreats = winningColumns(player.opponent)
      validColumns.flatMap: column =>
        play(column, player).toOption.map:
          case (next, row) =>
            TacticalOption(
              column = column,
              landingRow = row,
              winsNow = next.winner.contains(player),
              blocksImmediateThreat = opponentThreats.contains(column),
              opponentWinningReplies = next.winningColumns(player.opponent),
              ownWinningThreats = next.winningColumns(player),
              centerDistance = math.abs(3 - column),
            )

    def winner: Option[Player] =
      val directions = Vector((0, 1), (1, 0), (1, 1), (1, -1))
      (for
        row <- 0 until Rows
        column <- 0 until Columns
        player <- cells(row)(column).toVector
        (dr, dc) <- directions
        if (0 until 4).forall: offset =>
          val r = row + dr * offset
          val c = column + dc * offset
          r >= 0 && r < Rows && c >= 0 && c < Columns && cells(r)(c).contains(player)
      yield player).headOption

    def isFull: Boolean = validColumns.isEmpty

    def view: Vector[Vector[String]] =
      cells.map(_.map:
        case Some(Player.Jev) => "jev"
        case Some(Player.Llm) => "llm"
        case None             => "empty"
      )

    def promptView: String =
      val rows = cells.map(_.map:
        case Some(Player.Jev) => "J"
        case Some(Player.Llm) => "L"
        case None             => "."
      .mkString(" "))
      (rows :+ "0 1 2 3 4 5 6").mkString("\n")

  object Board:
    val empty: Board = Board(Vector.fill(Rows, Columns)(Option.empty[Player]))

  enum GameStatus derives JsonCodec:
    case Thinking, Won, Draw, Failed, Cancelled

  final case class MoveRecord(
    turn: Int,
    player: Player,
    column: Int,
    row: Int,
    durationMs: Long,
    note: String,
    inputTokens: Int,
    outputTokens: Int,
    estimatedCostUsd: BigDecimal,
  ) derives JsonCodec

  final case class GameSnapshot(
    id: String,
    modelId: String,
    modelLabel: String,
    inputUsdPerMillion: BigDecimal,
    outputUsdPerMillion: BigDecimal,
    bedrockCostUsd: BigDecimal,
    jevInputUsdPerMillion: BigDecimal,
    jevOutputUsdPerMillion: BigDecimal,
    jevCostUsd: BigDecimal,
    board: Vector[Vector[String]],
    status: GameStatus,
    currentPlayer: Option[Player],
    winner: Option[Player],
    moves: Vector[MoveRecord],
    message: String,
    createdAtMs: Long,
    turnStartedAtMs: Option[Long],
    finishedAtMs: Option[Long],
  ) derives JsonCodec

  final case class StartGameRequest(
    modelId: String,
    firstPlayer: String = "jev",
  ) derives JsonCodec

  final case class ApiError(error: String) derives JsonCodec
