import com.jamesward.zio_typesafe_ai.TypeSafeAI
import ConnectFour.*
import zio.*
import zio.http.{Client as HttpClient}
import zio.schema.{Schema, derived}
import zio.test.*
import zio.test.TestAspect.*

import scala.collection.immutable.VectorMap

object JevPromptExperimentSpec extends ZIOSpecDefault:
  private final case class Scenario(name: String, board: Board, expectedColumn: Int)

  private final case class ColumnState(
    currentPlayerPiece: String,
    opponentPiece: String,
    emptyCell: String,
    boardColumnsBottomToTop: Vector[String],
  ) derives Schema

  private final case class MatrixState(
    currentPlayerPiece: String,
    opponentPiece: String,
    emptyCell: String,
    boardRowsTopToBottom: Vector[Vector[String]],
  ) derives Schema

  private final case class LabeledAsciiState(
    currentPlayerPiece: String,
    opponentPiece: String,
    emptyCell: String,
    boardRowsWithCoordinates: String,
  ) derives Schema

  private final case class CandidateDescription(
    action: String,
    resultingBoardRowsTopToBottom: Vector[String],
  ) derives Schema

  private final case class MatrixCandidateDescription(
    action: String,
    resultingBoardRowsTopToBottom: Vector[Vector[String]],
  ) derives Schema

  private final case class DiagonalCandidateDescription(
    action: String,
    risingDiagonalWindows: Vector[String],
    fallingDiagonalWindows: Vector[String],
  ) derives Schema

  private final case class CandidateEvaluation(
    candidateColumn: Int,
    resultingBoardRowsTopToBottom: Vector[String],
    question: String,
  ) derives Schema

  private final case class Decision(column: Int, confidence: Double, trace: String)
  private type RunVariant = (Board, HttpClient, String) => Task[Decision]
  private final case class Variant(name: String, run: RunVariant)

  private val conciseInstructions =
    """Which column should J play?
      |Each board row or column uses zero-based columns 0 through 6; . is empty.
      |A move drops J into the lowest empty cell of the selected column.
      |First choose a move that gives J four adjacent pieces now.
      |Otherwise choose the column that stops L from making four adjacent pieces on its next move.""".stripMargin

  private val committedPriorityWithoutFacts =
    """Choose exactly one playable column.
      |First take an immediate win.
      |Otherwise prevent every immediate opponent win when possible.
      |Avoid moves that give the opponent an immediate winning reply when a safe move exists.
      |Only after immediate wins and losses are resolved, prefer multiple future winning threats and central columns.""".stripMargin

  private val verboseDefenseInstructions = MoveRequest.Instructions(
    question = "Which playable column should J choose after checking both players' immediate winning drops?",
    rules = MoveRequest.instructions.rules ++ Vector(
      "An immediate winning drop is a playable column where one dropped piece completes four adjacent pieces",
      "A defensive block occupies the landing cell the opponent needs before the opponent can use it",
      "Check vertical, horizontal, rising diagonal, and falling diagonal groups of four",
      "Gravity matters for horizontal and diagonal threats: an empty winning cell matters only when a piece can land there",
      "Creating three J pieces is not useful if L can win on the very next drop",
      "When J cannot win immediately, preventing L's immediate win is mandatory before building any J threat",
    ),
    goalsInOrder = Vector(
      "Simulate one J drop in every playable column and choose a column that completes four J pieces if one exists",
      "If J has no immediate win, simulate one L drop in every playable column and identify every drop that would complete four L pieces",
      "Choose a J drop that occupies an identified L winning cell and leaves L with no immediate winning drop whenever possible",
      "Only when neither player has an immediate win, improve J's future position",
    ),
  )

  private val verboseDefenseText =
    """Choose the best playable column for J.
      |A move drops one piece into the lowest empty cell of its column.
      |Before considering offense, inspect every vertical, horizontal, rising-diagonal, and falling-diagonal line.
      |First simulate J in every playable column and take a move that forms four J pieces immediately.
      |If J cannot win now, simulate L in every playable column on the current board.
      |Any L drop that forms four L pieces is an immediate losing threat.
      |J must block such a threat by occupying its required landing cell before L can use it.
      |Do not prefer creating two or three J pieces over blocking a move that lets L win next turn.
      |Consider longer-term strategy only after confirming L has no immediate winning drop.""".stripMargin

  private val asciiWinningShapes =
    """In the shape examples below, X means four pieces belonging to the same player and dots are other cells.
      |Horizontal shape: X X X X
      |Vertical shape:
      |X
      |X
      |X
      |X
      |Rising diagonal shape:
      |. . . X
      |. . X .
      |. X . .
      |X . . .
      |Falling diagonal shape:
      |X . . .
      |. X . .
      |. . X .
      |. . . X
      |A win requires four X positions with no gaps. The examples show geometry only; gravity still determines whether a new piece can occupy the missing position.""".stripMargin

  private val winningShapeInstructions = MoveRequest.Instructions(
    question = MoveRequest.instructions.question,
    rules = MoveRequest.instructions.rules :+ asciiWinningShapes,
    goalsInOrder = MoveRequest.instructions.goalsInOrder,
  )

  private val labeledProductionInstructions = MoveRequest.Instructions(
    question = MoveRequest.instructions.question.replace(
      "`boardRowsTopToBottom`",
      "`boardRowsWithCoordinates`",
    ),
    rules = MoveRequest.instructions.rules.map(
      _.replace("`boardRowsTopToBottom`", "`boardRowsWithCoordinates`").replace(
        "Each row string has seven characters; character at zero-based index N belongs to column N",
        "Each labeled row has seven cells under columns 0 through 6",
      )
    ),
    goalsInOrder = MoveRequest.instructions.goalsInOrder,
  )

  private val labeledAsciiInstructions =
    verboseDefenseText + "\n\nUse `boardRowsWithCoordinates` as the complete current board."

  private val labeledAsciiWithShapesInstructions =
    labeledAsciiInstructions + "\n\n" + asciiWinningShapes

  private val labeledStructuredVerboseInstructions = MoveRequest.Instructions(
    question = verboseDefenseInstructions.question,
    rules = verboseDefenseInstructions.rules.map(
      _.replace("`boardRowsTopToBottom`", "`boardRowsWithCoordinates`").replace(
        "Each row string has seven characters; character at zero-based index N belongs to column N",
        "Each labeled row has seven cells under columns 0 through 6",
      )
    ),
    goalsInOrder = verboseDefenseInstructions.goalsInOrder,
  )

  private val exhaustiveDefenseInstructions = MoveRequest.Instructions(
    question = "Which playable column survives an exhaustive one-turn win and loss check?",
    rules = MoveRequest.instructions.rules,
    goalsInOrder = Vector(
      "Enumerate each playable column by checking that its top cell is empty",
      "For each playable column, copy the board and place J in that column's lowest empty cell",
      "Scan each copied board for four adjacent J pieces in all four directions; if found, choose a winning copy",
      "If no J copy wins, return to the original board and place L in each playable column one at a time",
      "Scan each L copy for four adjacent L pieces in all four directions and remember every threatening column",
      "For each possible J move, then simulate every L reply; reject a J move if any L reply completes four",
      "Choose a non-rejected J move that blocks all immediate L wins; evaluate future strategy only if no immediate threat exists",
    ),
  )

  private def playAll(moves: (Int, Player)*): Board =
    moves.foldLeft(Board.empty):
      case (board, (column, player)) => board.play(column, player).toOption.get._1

  private val scenarios = Vector(
    Scenario(
      "win-horizontal",
      playAll(0 -> Player.Jev, 1 -> Player.Jev, 2 -> Player.Jev),
      3,
    ),
    Scenario(
      "win-vertical",
      playAll(2 -> Player.Jev, 2 -> Player.Jev, 2 -> Player.Jev),
      2,
    ),
    Scenario(
      "block-horizontal",
      playAll(
        0 -> Player.Llm,
        6 -> Player.Jev,
        1 -> Player.Llm,
        6 -> Player.Jev,
        2 -> Player.Llm,
      ),
      3,
    ),
    Scenario(
      "block-vertical-reported-game",
      playAll(
        3 -> Player.Llm,
        0 -> Player.Jev,
        3 -> Player.Llm,
        0 -> Player.Jev,
        3 -> Player.Llm,
      ),
      3,
    ),
    Scenario(
      "block-supported-diagonal",
      playAll(
        5 -> Player.Llm,
        4 -> Player.Jev,
        4 -> Player.Llm,
        3 -> Player.Llm,
        3 -> Player.Jev,
        3 -> Player.Llm,
        2 -> Player.Jev,
        2 -> Player.Llm,
        2 -> Player.Jev,
      ),
      2,
    ),
    Scenario(
      "block-elevated-horizontal",
      playAll(
        1 -> Player.Jev,
        1 -> Player.Llm,
        2 -> Player.Llm,
        2 -> Player.Llm,
        3 -> Player.Jev,
        3 -> Player.Llm,
        4 -> Player.Llm,
      ),
      4,
    ),
    Scenario(
      "win-horizontal-gap",
      playAll(0 -> Player.Jev, 1 -> Player.Jev, 3 -> Player.Jev),
      2,
    ),
    Scenario(
      "win-vertical-right",
      playAll(5 -> Player.Jev, 5 -> Player.Jev, 5 -> Player.Jev),
      5,
    ),
    Scenario(
      "block-horizontal-right",
      playAll(
        4 -> Player.Llm,
        0 -> Player.Jev,
        5 -> Player.Llm,
        0 -> Player.Jev,
        6 -> Player.Llm,
      ),
      3,
    ),
    Scenario(
      "block-vertical-right",
      playAll(
        5 -> Player.Llm,
        0 -> Player.Jev,
        5 -> Player.Llm,
        0 -> Player.Jev,
        5 -> Player.Llm,
      ),
      5,
    ),
    Scenario(
      "block-supported-diagonal-mirror",
      playAll(
        1 -> Player.Llm,
        2 -> Player.Jev,
        2 -> Player.Llm,
        3 -> Player.Llm,
        3 -> Player.Jev,
        3 -> Player.Llm,
        4 -> Player.Jev,
        4 -> Player.Llm,
        4 -> Player.Jev,
      ),
      4,
    ),
    Scenario(
      "block-elevated-horizontal-left",
      playAll(
        2 -> Player.Llm,
        3 -> Player.Jev,
        3 -> Player.Llm,
        4 -> Player.Llm,
        4 -> Player.Llm,
        5 -> Player.Jev,
        5 -> Player.Llm,
      ),
      2,
    ),
    Scenario(
      "win-supported-diagonal",
      playAll(
        5 -> Player.Jev,
        4 -> Player.Llm,
        4 -> Player.Jev,
        3 -> Player.Jev,
        3 -> Player.Llm,
        3 -> Player.Jev,
        2 -> Player.Llm,
        2 -> Player.Jev,
        2 -> Player.Llm,
      ),
      2,
    ),
    Scenario(
      "win-supported-diagonal-mirror",
      playAll(
        1 -> Player.Jev,
        2 -> Player.Llm,
        2 -> Player.Jev,
        3 -> Player.Jev,
        3 -> Player.Llm,
        3 -> Player.Jev,
        4 -> Player.Llm,
        4 -> Player.Jev,
        4 -> Player.Llm,
      ),
      4,
    ),
  )

  private val columnIds =
    Vector.tabulate(ConnectFour.Columns)(column => s"column_$column")

  private def allColumnsCriteria: TypeSafeAI.ChoiceCriteria =
    TypeSafeAI.ChoiceCriteria(
      columnIds.map(_ -> (null: String | Null))*
    ).toOption.get

  private def columnsOrNoneCriteria: TypeSafeAI.ChoiceCriteria =
    TypeSafeAI.ChoiceCriteria(
      (columnIds.map(_ -> (null: String | Null)) :+ ("none" -> (null: String | Null)))*
    ).toOption.get

  private def describedColumnsCriteria: TypeSafeAI.ChoiceCriteria =
    TypeSafeAI.ChoiceCriteria(
      columnIds.zipWithIndex.map: (id, column) =>
        id -> s"Drop J into zero-based column $column"
      *
    ).toOption.get

  private def asciiCandidateBoardCriteria(board: Board): TypeSafeAI.ChoiceCriteria =
    TypeSafeAI.ChoiceCriteria(
      MoveCandidateBoards.from(board, Player.Jev)
        .map(candidate => candidate.choiceId -> candidate.description)*
    ).toOption.get

  private def asciiCandidateChecklistCriteria(board: Board): TypeSafeAI.ChoiceCriteria =
    TypeSafeAI.ChoiceCriteria(
      MoveCandidateBoards.from(board, Player.Jev).map: candidate =>
        candidate.choiceId ->
          s"""${candidate.description}
             |Evaluate this resulting board in order:
             |1. Check whether J already has four adjacent pieces.
             |2. If not, simulate one legal L drop in every column and reject this choice if any reply gives L four adjacent pieces.
             |3. Consider future positional quality only after both checks.""".stripMargin
      *
    ).toOption.get

  private def asciiCandidateWithRepliesCriteria(board: Board): TypeSafeAI.ChoiceCriteria =
    TypeSafeAI.ChoiceCriteria(
      MoveCandidateBoards.withOpponentReplies(board, Player.Jev)
        .map(candidate => candidate.choiceId -> candidate.description)*
    ).toOption.get

  private def matrixCandidateBoardCriteria(board: Board): TypeSafeAI.ChoiceCriteria =
    val options = VectorMap.from((0 until ConnectFour.Columns).map: column =>
      val resultingRows = board.play(column, Player.Jev).toOption
        .map(_._1.symbolRows)
        .getOrElse(Vector(Vector("column is full")))
      s"column_$column" -> (TypeSafeAI.Content(MatrixCandidateDescription(
        action = s"Drop J into zero-based column $column",
        resultingBoardRowsTopToBottom = resultingRows,
      )): TypeSafeAI.Content | Null)
    )
    TypeSafeAI.ChoiceCriteria.fromContent(options).toOption.get

  private def materializedWinningBoardCriteria(
    board: Board,
    player: Player,
  ): TypeSafeAI.ChoiceCriteria =
    val piece = player match
      case Player.Jev => "J"
      case Player.Llm => "L"
    val options = (0 until ConnectFour.Columns).map: column =>
      val resultingRows = board.play(column, player).toOption
        .map(_._1.symbolRows.map(_.mkString))
        .getOrElse(Vector("column is full"))
      s"column_$column" -> (TypeSafeAI.Content(CandidateDescription(
        action = s"Drop $piece into zero-based column $column",
        resultingBoardRowsTopToBottom = resultingRows,
      )): TypeSafeAI.Content | Null)
    TypeSafeAI.ChoiceCriteria.fromContent(VectorMap.from(
      options :+ ("none" -> (TypeSafeAI.Content(
        s"None of the supplied post-drop boards contains four adjacent $piece pieces"
      ): TypeSafeAI.Content | Null))
    )).toOption.get

  private def materializedDiagonalWindowCriteria(
    board: Board,
    player: Player,
  ): TypeSafeAI.ChoiceCriteria =
    val piece = player match
      case Player.Jev => "J"
      case Player.Llm => "L"
    val options = (0 until ConnectFour.Columns).map: column =>
      val description = board.play(column, player).toOption match
        case Some((result, _)) =>
          val rows = result.symbolRows
          val falling = for
            row <- 0 to ConnectFour.Rows - 4
            startColumn <- 0 to ConnectFour.Columns - 4
          yield
            val cells = (0 until 4).map(offset => rows(row + offset)(startColumn + offset))
            s"($row,$startColumn)-(${row + 3},${startColumn + 3}): ${cells.mkString(" ")}"
          val rising = for
            row <- 0 to ConnectFour.Rows - 4
            startColumn <- 3 until ConnectFour.Columns
          yield
            val cells = (0 until 4).map(offset => rows(row + offset)(startColumn - offset))
            s"($row,$startColumn)-(${row + 3},${startColumn - 3}): ${cells.mkString(" ")}"
          DiagonalCandidateDescription(
            action = s"Drop $piece into zero-based column $column",
            risingDiagonalWindows = rising.toVector,
            fallingDiagonalWindows = falling.toVector,
          )
        case None =>
          DiagonalCandidateDescription(
            action = s"Column $column is full",
            risingDiagonalWindows = Vector.empty,
            fallingDiagonalWindows = Vector.empty,
          )
      s"column_$column" -> (TypeSafeAI.Content(description): TypeSafeAI.Content | Null)
    TypeSafeAI.ChoiceCriteria.fromContent(VectorMap.from(
      options :+ ("none" -> (TypeSafeAI.Content(
        s"No supplied four-cell diagonal window is exactly $piece $piece $piece $piece"
      ): TypeSafeAI.Content | Null))
    )).toOption.get

  private def candidateBoardCriteria(board: Board): TypeSafeAI.ChoiceCriteria =
    val options = VectorMap.from(columnIds.zipWithIndex.map: (id, column) =>
      val resultingRows = board.play(column, Player.Jev).toOption
        .map(_._1.symbolRows.map(_.mkString))
        .getOrElse(Vector("column is full"))
      id -> (TypeSafeAI.Content(CandidateDescription(
        action = s"Drop J into zero-based column $column",
        resultingBoardRowsTopToBottom = resultingRows,
      )): TypeSafeAI.Content | Null)
    )
    TypeSafeAI.ChoiceCriteria.fromContent(options).toOption.get

  private def request[S: Schema](
    state: S,
    questions: List[(TypeSafeAI.QuestionId, TypeSafeAI.Question[?])],
    httpClient: HttpClient,
    apiKey: String,
  ): Task[Map[TypeSafeAI.QuestionId, TypeSafeAI.DynamicAnswer]] =
    for
      request <- ZIO.fromEither(TypeSafeAI.askDynamic(state, questions))
        .mapError(IllegalArgumentException(_))
      result <- request.run.provideLayer(
        ZLayer.succeed(httpClient) >>> TypeSafeAI.Client.layer(TypeSafeAI.ApiKey(apiKey))
      )
    yield result.answers

  private def choice(
    answers: Map[TypeSafeAI.QuestionId, TypeSafeAI.DynamicAnswer],
    id: String,
  ): Task[TypeSafeAI.ChoiceAnswer] =
    ZIO.fromOption(answers.get(TypeSafeAI.QuestionId(id)))
      .orElseFail(IllegalStateException(s"Jev did not return $id"))
      .flatMap:
        case TypeSafeAI.DynamicAnswer.Choice(value) => ZIO.succeed(value)
        case other => ZIO.fail(IllegalStateException(s"Unexpected Jev answer for $id: $other"))

  private def column(answer: TypeSafeAI.ChoiceAnswer): Task[Int] =
    ZIO.fromOption(answer.choice.stripPrefix("column_").toIntOption)
      .orElseFail(IllegalStateException(s"Invalid column choice ${answer.choice}"))

  private def noul(
    answers: Map[TypeSafeAI.QuestionId, TypeSafeAI.DynamicAnswer],
    id: String,
  ): Task[Double] =
    ZIO.fromOption(answers.get(TypeSafeAI.QuestionId(id)))
      .orElseFail(IllegalStateException(s"Jev did not return $id"))
      .flatMap:
        case TypeSafeAI.DynamicAnswer.Noul(value) => ZIO.succeed(value.unwrap)
        case other => ZIO.fail(IllegalStateException(s"Unexpected Jev answer for $id: $other"))

  private def ask[S: Schema, I: Schema](
    state: S,
    instructions: I,
    criteria: TypeSafeAI.ChoiceCriteria,
    httpClient: HttpClient,
    apiKey: String,
  ): Task[Decision] =
    for
      answers <- request(
        state,
        List(TypeSafeAI.QuestionId("move") -> TypeSafeAI.Question.Choice(instructions, criteria)),
        httpClient,
        apiKey,
      )
      answer <- choice(answers, "move")
      selected <- column(answer)
    yield Decision(selected, answer.confidence.unwrap, s"move=${answer.choice}")

  private def decomposed(board: Board, httpClient: HttpClient, apiKey: String): Task[Decision] =
    val data = MoveRequest.from(board, Player.Jev)
    val tacticalCriteria = columnsOrNoneCriteria
    val questions: List[(TypeSafeAI.QuestionId, TypeSafeAI.Question[?])] = List(
      TypeSafeAI.QuestionId("winning_move") -> TypeSafeAI.Question.Choice(
        "Which playable column makes J form four adjacent J pieces on this move? Choose none if no column does.",
        tacticalCriteria,
      ),
      TypeSafeAI.QuestionId("opponent_winning_move") -> TypeSafeAI.Question.Choice(
        "If L moved now, which playable column would make L form four adjacent L pieces? Choose none if no column does.",
        tacticalCriteria,
      ),
      TypeSafeAI.QuestionId("fallback_move") -> TypeSafeAI.Question.Choice(
        conciseInstructions,
        allColumnsCriteria,
      ),
    )
    for
      answers <- request(data.state, questions, httpClient, apiKey)
      winning <- choice(answers, "winning_move")
      opponent <- choice(answers, "opponent_winning_move")
      fallback <- choice(answers, "fallback_move")
      selectedAnswer =
        if winning.choice != "none" then winning
        else if opponent.choice != "none" then opponent
        else fallback
      selected <- column(selectedAnswer)
      trace = s"winning=${winning.choice},opponent=${opponent.choice},fallback=${fallback.choice}"
    yield Decision(selected, selectedAnswer.confidence.unwrap, trace)

  private def materializedChoiceRouter(
    board: Board,
    httpClient: HttpClient,
    apiKey: String,
  ): Task[Decision] =
    val questions: List[(TypeSafeAI.QuestionId, TypeSafeAI.Question[?])] = List(
      TypeSafeAI.QuestionId("materialized_j_win") -> TypeSafeAI.Question.Choice(
        "Inspect every supplied post-drop board exactly as shown. Which board contains four adjacent J pieces horizontally, vertically, or diagonally? Choose none only if no supplied board does.",
        materializedWinningBoardCriteria(board, Player.Jev),
      ),
      TypeSafeAI.QuestionId("materialized_l_win") -> TypeSafeAI.Question.Choice(
        "Inspect every supplied post-drop board exactly as shown. Which board contains four adjacent L pieces horizontally, vertically, or diagonally? Choose none only if no supplied board does.",
        materializedWinningBoardCriteria(board, Player.Llm),
      ),
      TypeSafeAI.QuestionId("fallback_move") -> TypeSafeAI.Question.Choice(
        labeledAsciiInstructions,
        allColumnsCriteria,
      ),
    )
    for
      answers <- request(labeledAsciiState(board), questions, httpClient, apiKey)
      current <- choice(answers, "materialized_j_win")
      opponent <- choice(answers, "materialized_l_win")
      fallback <- choice(answers, "fallback_move")
      selectedAnswer =
        if current.choice != "none" then current
        else if opponent.choice != "none" then opponent
        else fallback
      selected <- column(selectedAnswer)
      trace =
        s"materializedJ=${current.choice}@${current.confidence.unwrap}," +
          s"materializedL=${opponent.choice}@${opponent.confidence.unwrap}," +
          s"fallback=${fallback.choice}@${fallback.confidence.unwrap}"
    yield Decision(selected, selectedAnswer.confidence.unwrap, trace)

  private def materializedDirectionalChoiceRouter(
    board: Board,
    httpClient: HttpClient,
    apiKey: String,
  ): Task[Decision] =
    val diagonalDefinition =
      "Rows increase from top to bottom. Four diagonal cells have consecutive columns and row indexes that change by exactly +1 or -1 at every step; there are no gaps."
    val questions: List[(TypeSafeAI.QuestionId, TypeSafeAI.Question[?])] = List(
      TypeSafeAI.QuestionId("materialized_j_win") -> TypeSafeAI.Question.Choice(
        "Inspect every supplied post-drop board exactly as shown. Which board contains four adjacent J pieces horizontally, vertically, or diagonally? Choose none only if no supplied board does.",
        materializedWinningBoardCriteria(board, Player.Jev),
      ),
      TypeSafeAI.QuestionId("materialized_j_diagonal") -> TypeSafeAI.Question.Choice(
        s"$diagonalDefinition Which choice has a supplied diagonal window exactly equal to J J J J? Choose none if no choice does.",
        materializedDiagonalWindowCriteria(board, Player.Jev),
      ),
      TypeSafeAI.QuestionId("materialized_l_win") -> TypeSafeAI.Question.Choice(
        "Inspect every supplied post-drop board exactly as shown. Which board contains four adjacent L pieces horizontally, vertically, or diagonally? Choose none only if no supplied board does.",
        materializedWinningBoardCriteria(board, Player.Llm),
      ),
      TypeSafeAI.QuestionId("materialized_l_diagonal") -> TypeSafeAI.Question.Choice(
        s"$diagonalDefinition Which choice has a supplied diagonal window exactly equal to L L L L? Choose none if no choice does.",
        materializedDiagonalWindowCriteria(board, Player.Llm),
      ),
      TypeSafeAI.QuestionId("fallback_move") -> TypeSafeAI.Question.Choice(
        labeledAsciiInstructions,
        allColumnsCriteria,
      ),
    )
    for
      answers <- request(labeledAsciiState(board), questions, httpClient, apiKey)
      current <- choice(answers, "materialized_j_win")
      currentDiagonal <- choice(answers, "materialized_j_diagonal")
      opponent <- choice(answers, "materialized_l_win")
      opponentDiagonal <- choice(answers, "materialized_l_diagonal")
      fallback <- choice(answers, "fallback_move")
      selectedAnswer =
        if current.choice != "none" then current
        else if currentDiagonal.choice != "none" && currentDiagonal.confidence.unwrap >= 0.5 then currentDiagonal
        else if opponent.choice != "none" then opponent
        else if opponentDiagonal.choice != "none" && opponentDiagonal.confidence.unwrap >= 0.5 then opponentDiagonal
        else fallback
      selected <- column(selectedAnswer)
      trace =
        s"J=${current.choice}@${current.confidence.unwrap}," +
          s"Jdiag=${currentDiagonal.choice}@${currentDiagonal.confidence.unwrap}," +
          s"L=${opponent.choice}@${opponent.confidence.unwrap}," +
          s"Ldiag=${opponentDiagonal.choice}@${opponentDiagonal.confidence.unwrap}," +
          s"fallback=${fallback.choice}@${fallback.confidence.unwrap}"
    yield Decision(selected, selectedAnswer.confidence.unwrap, trace)

  private def intentRouted(
    board: Board,
    httpClient: HttpClient,
    apiKey: String,
    confidenceGated: Boolean,
  ): Task[Decision] =
    val data = MoveRequest.from(board, Player.Jev)
    val intentCriteria = TypeSafeAI.ChoiceCriteria(
      "win_now" -> "J can form four adjacent J pieces with one drop",
      "block_opponent" -> "J cannot win now and L can form four adjacent L pieces with its next drop",
      "positional_move" -> "Neither player can form four on its next drop",
    ).toOption.get
    val questions: List[(TypeSafeAI.QuestionId, TypeSafeAI.Question[?])] = List(
      TypeSafeAI.QuestionId("intent") -> TypeSafeAI.Question.Choice(
        "Which tactical situation describes the current board? Apply win_now before block_opponent.",
        intentCriteria,
      ),
      TypeSafeAI.QuestionId("winning_move") -> TypeSafeAI.Question.Choice(
        "Which playable column makes J form four adjacent J pieces on this move? Choose none if no column does.",
        columnsOrNoneCriteria,
      ),
      TypeSafeAI.QuestionId("blocking_move") -> TypeSafeAI.Question.Choice(
        "Which playable column would make L form four adjacent L pieces if L moved now? Choose none if no column does.",
        columnsOrNoneCriteria,
      ),
      TypeSafeAI.QuestionId("fallback_move") -> TypeSafeAI.Question.Choice(
        conciseInstructions,
        allColumnsCriteria,
      ),
    )
    for
      answers <- request(data.state, questions, httpClient, apiKey)
      intent <- choice(answers, "intent")
      winning <- choice(answers, "winning_move")
      blocking <- choice(answers, "blocking_move")
      fallback <- choice(answers, "fallback_move")
      specialist = intent.choice match
        case "win_now" => winning
        case "block_opponent" => blocking
        case _ => fallback
      routed =
        if specialist.choice == "none" then fallback
        else if confidenceGated && (intent.confidence.unwrap < 0.5 || specialist.confidence.unwrap < 0.4) then fallback
        else specialist
      selected <- column(routed)
      trace =
        s"intent=${intent.choice}@${intent.confidence.unwrap}," +
          s"win=${winning.choice}@${winning.confidence.unwrap}," +
          s"block=${blocking.choice}@${blocking.confidence.unwrap}," +
          s"fallback=${fallback.choice}@${fallback.confidence.unwrap}"
    yield Decision(selected, routed.confidence.unwrap, trace)

  private def candidateCompositeScoring(
    board: Board,
    httpClient: HttpClient,
    apiKey: String,
  ): Task[Decision] =
    val data = MoveRequest.from(board, Player.Jev)
    val resultBoards = columnIds.indices.map: column =>
      column -> board.play(column, Player.Jev).toOption
        .map(_._1.symbolRows.map(_.mkString))
        .getOrElse(Vector("column is full"))
    val questions = resultBoards.toList.flatMap: (column, rows) =>
      List(
        TypeSafeAI.QuestionId(s"candidate_wins_$column") -> TypeSafeAI.Question.Noul(
          CandidateEvaluation(
            column,
            rows,
            "Does `resultingBoardRowsTopToBottom` contain four adjacent J pieces?",
          )
        ),
        TypeSafeAI.QuestionId(s"opponent_wins_next_$column") -> TypeSafeAI.Question.Noul(
          CandidateEvaluation(
            column,
            rows,
            "From `resultingBoardRowsTopToBottom`, can L form four adjacent L pieces with one legal drop?",
          )
        ),
      )
    for
      answers <- request(data.state, questions, httpClient, apiKey)
      dimensions <- ZIO.foreach(columnIds.indices): column =>
        for
          wins <- noul(answers, s"candidate_wins_$column")
          opponentRisk <- noul(answers, s"opponent_wins_next_$column")
        yield (column, wins, opponentRisk, (3.0 * wins) + (1.0 - opponentRisk))
      selected = dimensions.maxBy(_._4)
      trace = dimensions.map((column, wins, risk, score) =>
        f"$column:w=$wins%.2f,r=$risk%.2f,s=$score%.2f"
      ).mkString(",")
    yield Decision(selected._1, selected._4 / 4.0, trace)

  private def perColumnNouls(board: Board, httpClient: HttpClient, apiKey: String): Task[Decision] =
    val data = MoveRequest.from(board, Player.Jev)
    val criteria = TypeSafeAI.NoulCriteria(
      whenTrue = "The hypothetical drop creates four adjacent pieces for the named player",
      whenFalse = "The hypothetical drop does not create four adjacent pieces for the named player",
    )
    val tacticalQuestions = columnIds.indices.toList.flatMap: column =>
      List(
        TypeSafeAI.QuestionId(s"current_wins_$column") -> TypeSafeAI.Question.Noul(
          s"If J drops one piece into column $column according to gravity, does the resulting board contain four adjacent J pieces?",
          criteria,
        ),
        TypeSafeAI.QuestionId(s"opponent_wins_$column") -> TypeSafeAI.Question.Noul(
          s"If L drops one piece into column $column according to gravity, does the resulting board contain four adjacent L pieces?",
          criteria,
        ),
      )
    val fallbackQuestion = TypeSafeAI.QuestionId("fallback_move") -> TypeSafeAI.Question.Choice(
      conciseInstructions,
      allColumnsCriteria,
    )
    for
      answers <- request(data.state, tacticalQuestions :+ fallbackQuestion, httpClient, apiKey)
      currentScores <- ZIO.foreach(columnIds.indices)(column =>
        noul(answers, s"current_wins_$column").map(column -> _)
      )
      opponentScores <- ZIO.foreach(columnIds.indices)(column =>
        noul(answers, s"opponent_wins_$column").map(column -> _)
      )
      fallback <- choice(answers, "fallback_move")
      fallbackColumn <- column(fallback)
      currentBest = currentScores.maxBy(_._2)
      opponentBest = opponentScores.maxBy(_._2)
      decision =
        if currentBest._2 >= 0.5 then Decision(currentBest._1, currentBest._2, "current=" + currentScores.mkString(","))
        else if opponentBest._2 >= 0.5 then Decision(opponentBest._1, opponentBest._2, "opponent=" + opponentScores.mkString(","))
        else Decision(fallbackColumn, fallback.confidence.unwrap, "fallback=" + fallback.choice)
    yield decision

  private def materializedPerColumnNouls(
    board: Board,
    httpClient: HttpClient,
    apiKey: String,
  ): Task[Decision] =
    val criteria = TypeSafeAI.NoulCriteria(
      whenTrue = "The supplied resulting board contains four adjacent pieces for the named player",
      whenFalse = "The supplied resulting board does not contain four adjacent pieces for the named player",
    )
    def resultingRows(column: Int, player: Player): Vector[String] =
      board.play(column, player).toOption
        .map(_._1.symbolRows.map(_.mkString))
        .getOrElse(Vector("column is full"))
    val tacticalQuestions = columnIds.indices.toList.flatMap: column =>
      List(
        TypeSafeAI.QuestionId(s"materialized_j_wins_$column") -> TypeSafeAI.Question.Noul(
          CandidateEvaluation(
            column,
            resultingRows(column, Player.Jev),
            "Does this exact resulting board contain four adjacent J pieces horizontally, vertically, or diagonally?",
          ),
          criteria,
        ),
        TypeSafeAI.QuestionId(s"materialized_l_wins_$column") -> TypeSafeAI.Question.Noul(
          CandidateEvaluation(
            column,
            resultingRows(column, Player.Llm),
            "Does this exact resulting board contain four adjacent L pieces horizontally, vertically, or diagonally?",
          ),
          criteria,
        ),
      )
    val fallbackQuestion = TypeSafeAI.QuestionId("fallback_move") -> TypeSafeAI.Question.Choice(
      labeledAsciiInstructions,
      allColumnsCriteria,
    )
    for
      answers <- request(labeledAsciiState(board), tacticalQuestions :+ fallbackQuestion, httpClient, apiKey)
      currentScores <- ZIO.foreach(columnIds.indices)(column =>
        noul(answers, s"materialized_j_wins_$column").map(column -> _)
      )
      opponentScores <- ZIO.foreach(columnIds.indices)(column =>
        noul(answers, s"materialized_l_wins_$column").map(column -> _)
      )
      fallback <- choice(answers, "fallback_move")
      fallbackColumn <- column(fallback)
      currentBest = currentScores.maxBy(_._2)
      opponentBest = opponentScores.maxBy(_._2)
      decision =
        if currentBest._2 >= 0.5 then Decision(currentBest._1, currentBest._2, "materialized-current=" + currentScores.mkString(","))
        else if opponentBest._2 >= 0.5 then Decision(opponentBest._1, opponentBest._2, "materialized-opponent=" + opponentScores.mkString(","))
        else Decision(fallbackColumn, fallback.confidence.unwrap, "materialized-fallback=" + fallback.choice)
    yield decision

  private def stateAndTransitionProbe(
    scenario: Scenario,
    httpClient: HttpClient,
    apiKey: String,
  ): Task[(Double, Double)] =
    val data = MoveRequest.from(scenario.board, Player.Jev)
    val observedPatternQuestion = scenario.name match
      case "win-horizontal" => "Are characters 0, 1, and 2 of the bottom row all J?"
      case "win-vertical" => "Are the bottom three characters of column 2 all J?"
      case "block-horizontal" => "Are characters 0, 1, and 2 of the bottom row all L?"
      case "block-vertical-reported-game" => "Are the bottom three characters of column 3 all L?"
    val hypotheticalPiece = if scenario.name.startsWith("win-") then "J" else "L"
    val hypotheticalQuestion =
      s"If $hypotheticalPiece drops into column ${scenario.expectedColumn}, does that create four adjacent $hypotheticalPiece pieces?"
    val questions: List[(TypeSafeAI.QuestionId, TypeSafeAI.Question[?])] = List(
      TypeSafeAI.QuestionId("observed_pattern") -> TypeSafeAI.Question.Noul(observedPatternQuestion),
      TypeSafeAI.QuestionId("hypothetical_drop_wins") -> TypeSafeAI.Question.Noul(hypotheticalQuestion),
    )
    for
      answers <- request(data.state, questions, httpClient, apiKey)
      observed <- noul(answers, "observed_pattern")
      hypothetical <- noul(answers, "hypothetical_drop_wins")
    yield observed -> hypothetical

  private def matrixState(board: Board): MatrixState =
    MatrixState(
      currentPlayerPiece = "J",
      opponentPiece = "L",
      emptyCell = ".",
      boardRowsTopToBottom = board.symbolRows,
    )

  private def labeledAsciiState(board: Board): LabeledAsciiState =
    val rows = board.symbolRows.zipWithIndex
      .map((row, index) => s"row $index: ${row.mkString(" ")}")
      .mkString("\n")
    LabeledAsciiState(
      currentPlayerPiece = "J",
      opponentPiece = "L",
      emptyCell = ".",
      boardRowsWithCoordinates = s"$rows\ncolumns: 0 1 2 3 4 5 6",
    )

  private def committedTextState(board: Board): String =
    s"""You are Jev playing Connect Four as J against L. It is your turn.
       |Columns are numbered 0 through 6. A piece falls to the lowest empty row in its column.
       |Rows are numbered 0 through 5 from top to bottom. A full column cannot be played.
       |The first player with four adjacent pieces horizontally, vertically, or diagonally wins.
       |The board below is the complete current game state: J is Jev, L is the opponent, and . is empty.
       |
       |${board.promptView}""".stripMargin

  private def textState(board: Board): String =
    s"""Current player: J
       |Opponent: L
       |Empty cell: .
       |Board rows from top to bottom:
       |${board.symbolRows.map(_.mkString).mkString("\n")}
       |Columns: 0123456""".stripMargin

  private def columnState(board: Board): ColumnState =
    ColumnState(
      currentPlayerPiece = "J",
      opponentPiece = "L",
      emptyCell = ".",
      boardColumnsBottomToTop = board.symbolRows.transpose.map(_.reverse.mkString),
    )

  private val variants = Vector(
    Variant(
      "matrix-state-2d",
      (board, client, key) =>
        ask(matrixState(board), MoveRequest.instructions, allColumnsCriteria, client, key),
    ),
    Variant(
      "matrix-state-2d-with-win-shapes",
      (board, client, key) =>
        ask(matrixState(board), winningShapeInstructions, allColumnsCriteria, client, key),
    ),
    Variant(
      "matrix-post-move-choice-boards",
      (board, client, key) =>
        ask(matrixState(board), MoveRequest.instructions, matrixCandidateBoardCriteria(board), client, key),
    ),
    Variant(
      "row-labeled-ascii-production-guidance",
      (board, client, key) =>
        ask(labeledAsciiState(board), labeledProductionInstructions, allColumnsCriteria, client, key),
    ),
    Variant(
      "row-labeled-ascii-state",
      (board, client, key) =>
        ask(labeledAsciiState(board), labeledAsciiInstructions, allColumnsCriteria, client, key),
    ),
    Variant(
      "row-labeled-ascii-with-win-shapes",
      (board, client, key) =>
        ask(labeledAsciiState(board), labeledAsciiWithShapesInstructions, allColumnsCriteria, client, key),
    ),
    Variant(
      "row-labeled-structured-verbose",
      (board, client, key) =>
        ask(labeledAsciiState(board), labeledStructuredVerboseInstructions, allColumnsCriteria, client, key),
    ),
    Variant(
      "row-labeled-verbose-ascii-candidates",
      (board, client, key) =>
        ask(labeledAsciiState(board), labeledAsciiInstructions, asciiCandidateBoardCriteria(board), client, key),
    ),
    Variant(
      "row-labeled-verbose-candidate-checklist",
      (board, client, key) =>
        ask(labeledAsciiState(board), labeledAsciiInstructions, asciiCandidateChecklistCriteria(board), client, key),
    ),
    Variant(
      "row-labeled-verbose-opponent-replies",
      (board, client, key) =>
        ask(labeledAsciiState(board), labeledAsciiInstructions, asciiCandidateWithRepliesCriteria(board), client, key),
    ),
    Variant(
      "production-structured",
      (board, client, key) =>
        val data = MoveRequest.from(board, Player.Jev)
        ask(data.state, data.instructions, allColumnsCriteria, client, key),
    ),
    Variant(
      "compact-rows-concise-string-instructions",
      (board, client, key) =>
        val data = MoveRequest.from(board, Player.Jev)
        ask(data.state, conciseInstructions, allColumnsCriteria, client, key),
    ),
    Variant(
      "text-state-structured-instructions",
      (board, client, key) =>
        ask(textState(board), MoveRequest.instructions, allColumnsCriteria, client, key),
    ),
    Variant(
      "text-state-concise-string-instructions",
      (board, client, key) =>
        ask(textState(board), conciseInstructions, allColumnsCriteria, client, key),
    ),
    Variant(
      "bottom-up-columns-concise-instructions",
      (board, client, key) =>
        ask(columnState(board), conciseInstructions, allColumnsCriteria, client, key),
    ),
    Variant(
      "structured-with-described-options",
      (board, client, key) =>
        val data = MoveRequest.from(board, Player.Jev)
        ask(data.state, data.instructions, describedColumnsCriteria, client, key),
    ),
    Variant(
      "committed-priority-no-facts",
      (board, client, key) =>
        val data = MoveRequest.from(board, Player.Jev)
        ask(data.state, committedPriorityWithoutFacts, allColumnsCriteria, client, key),
    ),
    Variant(
      "committed-text-state-no-facts",
      (board, client, key) =>
        ask(committedTextState(board), committedPriorityWithoutFacts, allColumnsCriteria, client, key),
    ),
    Variant(
      "verbose-defense-structured",
      (board, client, key) =>
        val data = MoveRequest.from(board, Player.Jev)
        ask(data.state, verboseDefenseInstructions, allColumnsCriteria, client, key),
    ),
    Variant(
      "verbose-defense-string",
      (board, client, key) =>
        val data = MoveRequest.from(board, Player.Jev)
        ask(data.state, verboseDefenseText, allColumnsCriteria, client, key),
    ),
    Variant(
      "exhaustive-defense-structured",
      (board, client, key) =>
        val data = MoveRequest.from(board, Player.Jev)
        ask(data.state, exhaustiveDefenseInstructions, allColumnsCriteria, client, key),
    ),
    Variant(
      "verbose-defense-ascii-candidates",
      (board, client, key) =>
        val data = MoveRequest.from(board, Player.Jev)
        ask(data.state, verboseDefenseInstructions, asciiCandidateBoardCriteria(board), client, key),
    ),
    Variant(
      "verbose-defense-with-opponent-reply-boards",
      (board, client, key) =>
        val data = MoveRequest.from(board, Player.Jev)
        ask(data.state, verboseDefenseInstructions, asciiCandidateWithRepliesCriteria(board), client, key),
    ),
    Variant(
      "ascii-post-move-with-opponent-replies",
      (board, client, key) =>
        val data = MoveRequest.from(board, Player.Jev)
        ask(data.state, data.instructions, asciiCandidateWithRepliesCriteria(board), client, key),
    ),
    Variant(
      "ascii-post-move-choice-boards",
      (board, client, key) =>
        val data = MoveRequest.from(board, Player.Jev)
        ask(data.state, data.instructions, asciiCandidateBoardCriteria(board), client, key),
    ),
    Variant(
      "structured-with-candidate-result-boards",
      (board, client, key) =>
        val data = MoveRequest.from(board, Player.Jev)
        ask(data.state, data.instructions, candidateBoardCriteria(board), client, key),
    ),
    Variant("decomposed-parallel-questions", decomposed),
    Variant("materialized-choice-router", materializedChoiceRouter),
    Variant("materialized-directional-choice-router", materializedDirectionalChoiceRouter),
    Variant(
      "intent-routed-fan-out",
      (board, client, key) => intentRouted(board, client, key, confidenceGated = false),
    ),
    Variant(
      "confidence-gated-intent-routing",
      (board, client, key) => intentRouted(board, client, key, confidenceGated = true),
    ),
    Variant("candidate-composite-scoring", candidateCompositeScoring),
    Variant("per-column-atomic-nouls", perColumnNouls),
    Variant("materialized-per-column-nouls", materializedPerColumnNouls),
  )

  private val requestedVariantNames =
    sys.env.get("JEV_PROMPT_VARIANTS")
      .map(_.split(',').iterator.map(_.trim).filter(_.nonEmpty).toSet)

  private val requestedScenarioNames =
    sys.env.get("JEV_PROMPT_SCENARIOS")
      .map(_.split(',').iterator.map(_.trim).filter(_.nonEmpty).toSet)

  private val selectedScenarios =
    requestedScenarioNames.fold(scenarios)(names => scenarios.filter(scenario => names.contains(scenario.name)))

  private val selectedVariants =
    requestedVariantNames.fold(variants)(names => variants.filter(variant => names.contains(variant.name)))

  private val includeStateProbes =
    requestedVariantNames.forall(_.contains("state-transition-probes"))

  private def apiKey: Task[String] =
    ZIO.systemWith(_.env("TYPESAFE_API_KEY")).orDie.flatMap:
      case Some(value) if value.trim.nonEmpty => ZIO.succeed(value.trim)
      case _ => ZIO.fail(IllegalStateException("TYPESAFE_API_KEY is required"))

  def spec =
    val fixtureSuite = suite("scenario-fixtures")(
      selectedScenarios.map: scenario =>
        test(s"${scenario.name} has one forced tactical column") {
          val currentWins = scenario.board.winningColumns(Player.Jev)
          val opponentWins = scenario.board.winningColumns(Player.Llm)
          assertTrue(
            if scenario.name.startsWith("win-") then
              currentWins == Vector(scenario.expectedColumn) && opponentWins.isEmpty
            else
              currentWins.isEmpty && opponentWins == Vector(scenario.expectedColumn)
          )
        }
    )
    val stateProbeSuite = suite("state-transition-probes")(
      selectedScenarios.map: scenario =>
        test(s"${scenario.name} reads pattern and winning transition") {
          for
            client <- ZIO.service[HttpClient]
            key <- apiKey
            result <- stateAndTransitionProbe(scenario, client, key)
            (observed, hypothetical) = result
            _ <- ZIO.logInfo(
              s"JEV_DIAGNOSTIC scenario=${scenario.name} observedPattern=$observed " +
                s"hypotheticalDropWins=$hypothetical"
            )
          yield assertTrue(observed >= 0.5, hypothetical >= 0.5)
        }
    )
    val variantSuites = selectedVariants.map: variant =>
      suite(variant.name)(
        selectedScenarios.map: scenario =>
          test(s"${scenario.name} chooses column ${scenario.expectedColumn}") {
            for
              client <- ZIO.service[HttpClient]
              key <- apiKey
              decision <- variant.run(scenario.board, client, key)
              _ <- ZIO.logInfo(
                s"JEV_PROMPT_EXPERIMENT variant=${variant.name} scenario=${scenario.name} " +
                  s"expected=${scenario.expectedColumn} selected=${decision.column} " +
                  s"confidence=${decision.confidence} trace=${decision.trace}"
              )
            yield assertTrue(decision.column == scenario.expectedColumn)
          }
      )
    val suites = Vector(fixtureSuite) ++ Option.when(includeStateProbes)(stateProbeSuite).toVector ++ variantSuites
    suite("Jev prompt experiments")(suites).provideShared(
      HttpClient.default,
    ) @@ ifEnvSet("RUN_JEV_PROMPT_EXPERIMENTS") @@ ifEnvSet("TYPESAFE_API_KEY") @@
      withLiveSystem @@ withLiveClock @@ timeout(5.minutes) @@ sequential
