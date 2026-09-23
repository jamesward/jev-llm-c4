import com.jamesward.zio_typesafe_ai.TypeSafeAI
import ConnectFour.*
import zio.*
import zio.http.{Client as HttpClient}
import zio.test.*
import zio.test.TestAspect.*

object JevUnderstandingDiagnosticSpec extends ZIOSpecDefault:
  private final case class Scenario(name: String, board: Board)

  private final case class Check(
    category: String,
    id: String,
    expected: String,
    actual: String,
    signal: Double,
  ):
    def isCorrect: Boolean = expected == actual

  private val categories = Vector(
    "positions",
    "legality",
    "gravity",
    "own-one-step-win",
    "opponent-one-step-win",
    "opponent-reply-after-candidate",
    "intent",
  )

  private val cellCriteria = TypeSafeAI.ChoiceCriteria(
    "J" -> "The cell contains J",
    "L" -> "The cell contains L",
    "empty" -> "The cell contains the emptyCell character",
  ).toOption.get

  private val landingRowCriteria = TypeSafeAI.ChoiceCriteria(
    ((0 until ConnectFour.Rows).map(row => s"row_$row" -> (null: String | Null)) :+
      ("none" -> (null: String | Null)))*
  ).toOption.get

  private val intentCriteria = TypeSafeAI.ChoiceCriteria(
    "j_wins_now" -> "J can complete four adjacent J pieces with one legal drop",
    "must_block_l" -> "J cannot win now and L can complete four adjacent L pieces with its next legal drop",
    "no_immediate_tactic" -> "Neither player can complete four with its next legal drop",
  ).toOption.get

  private val questions: List[(TypeSafeAI.QuestionId, TypeSafeAI.Question[?])] =
    val cells = (for
      row <- 0 until ConnectFour.Rows
      column <- 0 until ConnectFour.Columns
    yield
      TypeSafeAI.QuestionId(s"cell_${row}_$column") -> TypeSafeAI.Question.Choice(
        s"In `boardRowsTopToBottom`, what occupies zero-based row $row and column $column?",
        cellCriteria,
      )
    ).toList

    val playable = (0 until ConnectFour.Columns).map: column =>
      TypeSafeAI.QuestionId(s"playable_$column") -> TypeSafeAI.Question.Noul(
        s"Is column $column playable now? It is playable exactly when character $column of the first row is `emptyCell`."
      )

    val landingRows = (0 until ConnectFour.Columns).map: column =>
      TypeSafeAI.QuestionId(s"landing_$column") -> TypeSafeAI.Question.Choice(
        s"Rows are indexed 0 at the top through 5 at the bottom. If J is dropped into column $column, it occupies the empty cell with the greatest row index. Which row receives it? Choose none if the column is full.",
        landingRowCriteria,
      )

    val ownWins = (0 until ConnectFour.Columns).map: column =>
      TypeSafeAI.QuestionId(s"j_wins_$column") -> TypeSafeAI.Question.Noul(
        s"A legal drop occupies the empty cell with the greatest row index in its column. If J is dropped into column $column, does the resulting board contain four adjacent J pieces horizontally, vertically, or diagonally?"
      )

    val opponentWins = (0 until ConnectFour.Columns).map: column =>
      TypeSafeAI.QuestionId(s"l_wins_$column") -> TypeSafeAI.Question.Noul(
        s"A legal drop occupies the empty cell with the greatest row index in its column. If L is dropped into column $column, does the resulting board contain four adjacent L pieces horizontally, vertically, or diagonally?"
      )

    val opponentReplies = (0 until ConnectFour.Columns).map: column =>
      TypeSafeAI.QuestionId(s"l_reply_after_j_$column") -> TypeSafeAI.Question.Noul(
        s"Apply gravity twice: J first occupies the greatest empty row index in column $column, then L may occupy the greatest empty row index in any playable column. Can L complete four adjacent L pieces with that one reply? Answer no if J's drop already wins or column $column is full."
      )

    val intent = TypeSafeAI.QuestionId("intent") -> TypeSafeAI.Question.Choice(
      "First check whether one gravity drop lets J make four; otherwise check whether one gravity drop lets L make four. Which immediate tactical condition has priority for J?",
      intentCriteria,
    )

    cells ++ playable ++ landingRows ++ ownWins ++ opponentWins ++ opponentReplies :+ intent

  private def playAll(moves: (Int, Player)*): Board =
    moves.foldLeft(Board.empty):
      case (board, (column, player)) => board.play(column, player).toOption.get._1

  private val scenarios = Vector(
    Scenario("empty", Board.empty),
    Scenario(
      "full-column-zero",
      playAll(
        0 -> Player.Jev,
        0 -> Player.Llm,
        0 -> Player.Jev,
        0 -> Player.Llm,
        0 -> Player.Jev,
        0 -> Player.Llm,
        1 -> Player.Llm,
      ),
    ),
    Scenario(
      "win-horizontal",
      playAll(0 -> Player.Jev, 1 -> Player.Jev, 2 -> Player.Jev),
    ),
    Scenario(
      "win-vertical",
      playAll(2 -> Player.Jev, 2 -> Player.Jev, 2 -> Player.Jev),
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
    ),
  )

  private val selectedScenarios =
    sys.env.get("JEV_DIAGNOSTIC_SCENARIOS")
      .map(_.split(',').iterator.map(_.trim).filter(_.nonEmpty).toSet)
      .fold(scenarios)(names => scenarios.filter(scenario => names.contains(scenario.name)))

  private def apiKey: Task[String] =
    ZIO.systemWith(_.env("TYPESAFE_API_KEY")).orDie.flatMap:
      case Some(value) if value.trim.nonEmpty => ZIO.succeed(value.trim)
      case _ => ZIO.fail(IllegalStateException("TYPESAFE_API_KEY is required"))

  private def run(
    scenario: Scenario,
    httpClient: HttpClient,
    key: String,
  ): Task[TypeSafeAI.Result[Map[TypeSafeAI.QuestionId, TypeSafeAI.DynamicAnswer]]] =
    val state = MoveRequest.from(scenario.board, Player.Jev).state
    for
      request <- ZIO.fromEither(TypeSafeAI.askDynamic(state, questions))
        .mapError(IllegalArgumentException(_))
      result <- request.run.provideLayer(
        ZLayer.succeed(httpClient) >>> TypeSafeAI.Client.layer(TypeSafeAI.ApiKey(key))
      )
    yield result

  private def choiceObservation(
    answers: Map[TypeSafeAI.QuestionId, TypeSafeAI.DynamicAnswer],
    id: String,
  ): Either[String, (String, Double)] =
    answers.get(TypeSafeAI.QuestionId(id)).toRight(s"missing $id").flatMap:
      case TypeSafeAI.DynamicAnswer.Choice(answer) =>
        Right(answer.choice -> answer.confidence.unwrap)
      case other => Left(s"$id returned $other")

  private def noulObservation(
    answers: Map[TypeSafeAI.QuestionId, TypeSafeAI.DynamicAnswer],
    id: String,
  ): Either[String, Double] =
    answers.get(TypeSafeAI.QuestionId(id)).toRight(s"missing $id").flatMap:
      case TypeSafeAI.DynamicAnswer.Noul(value) => Right(value.unwrap)
      case other => Left(s"$id returned $other")

  private def sequence(values: Vector[Either[String, Check]]): Either[String, Vector[Check]] =
    values.foldLeft[Either[String, Vector[Check]]](Right(Vector.empty)):
      case (acc, value) =>
        for
          checks <- acc
          check <- value
        yield checks :+ check

  private def choiceCheck(
    category: String,
    id: String,
    expected: String,
    answers: Map[TypeSafeAI.QuestionId, TypeSafeAI.DynamicAnswer],
  ): Either[String, Check] =
    choiceObservation(answers, id).map: (actual, confidence) =>
      Check(category, id, expected, actual, confidence)

  private def noulCheck(
    category: String,
    id: String,
    expected: Boolean,
    answers: Map[TypeSafeAI.QuestionId, TypeSafeAI.DynamicAnswer],
  ): Either[String, Check] =
    noulObservation(answers, id).map: probability =>
      Check(category, id, expected.toString, (probability >= 0.5).toString, probability)

  private def expectedIntent(board: Board): String =
    if board.winningColumns(Player.Jev).nonEmpty then "j_wins_now"
    else if board.winningColumns(Player.Llm).nonEmpty then "must_block_l"
    else "no_immediate_tactic"

  private def checks(
    scenario: Scenario,
    answers: Map[TypeSafeAI.QuestionId, TypeSafeAI.DynamicAnswer],
  ): Either[String, Vector[Check]] =
    val board = scenario.board
    val cellChecks = (for
      row <- 0 until ConnectFour.Rows
      column <- 0 until ConnectFour.Columns
    yield
      val symbol = board.symbolRows(row)(column)
      val expected = if symbol == "." then "empty" else symbol
      choiceCheck("positions", s"cell_${row}_$column", expected, answers)
    ).toVector

    val legalityChecks = (0 until ConnectFour.Columns).toVector.map: column =>
      noulCheck("legality", s"playable_$column", board.validColumns.contains(column), answers)

    val gravityChecks = (0 until ConnectFour.Columns).toVector.map: column =>
      val expected = board.play(column, Player.Jev).toOption
        .map((_, row) => s"row_$row")
        .getOrElse("none")
      choiceCheck("gravity", s"landing_$column", expected, answers)

    val ownWinChecks = (0 until ConnectFour.Columns).toVector.map: column =>
      val expected = board.play(column, Player.Jev).toOption
        .exists((result, _) => result.winner.contains(Player.Jev))
      noulCheck("own-one-step-win", s"j_wins_$column", expected, answers)

    val opponentWinChecks = (0 until ConnectFour.Columns).toVector.map: column =>
      val expected = board.play(column, Player.Llm).toOption
        .exists((result, _) => result.winner.contains(Player.Llm))
      noulCheck("opponent-one-step-win", s"l_wins_$column", expected, answers)

    val replyChecks = (0 until ConnectFour.Columns).toVector.map: column =>
      val expected = board.play(column, Player.Jev).toOption.exists: (result, _) =>
        result.winner.isEmpty && result.winningColumns(Player.Llm).nonEmpty
      noulCheck("opponent-reply-after-candidate", s"l_reply_after_j_$column", expected, answers)

    val intentCheck = choiceCheck("intent", "intent", expectedIntent(board), answers)

    sequence(
      cellChecks ++ legalityChecks ++ gravityChecks ++ ownWinChecks ++
        opponentWinChecks ++ replyChecks :+ intentCheck
    )

  def spec =
    val scenarioTests = selectedScenarios.map: scenario =>
      test(s"${scenario.name} understanding") {
        for
          client <- ZIO.service[HttpClient]
          key <- apiKey
          result <- run(scenario, client, key)
          evaluated <- ZIO.fromEither(checks(scenario, result.answers))
            .mapError(IllegalStateException(_))
          _ <- ZIO.foreachDiscard(categories): category =>
            val categoryChecks = evaluated.filter(_.category == category)
            val correct = categoryChecks.count(_.isCorrect)
            ZIO.logInfo(
              s"JEV_UNDERSTANDING scenario=${scenario.name} category=$category " +
                s"correct=$correct/${categoryChecks.size}"
            )
          _ <- ZIO.foreachDiscard(evaluated.filterNot(_.isCorrect)): check =>
            ZIO.logWarning(
              s"JEV_UNDERSTANDING_MISMATCH scenario=${scenario.name} category=${check.category} " +
                s"id=${check.id} expected=${check.expected} actual=${check.actual} signal=${check.signal}"
            )
          _ <- ZIO.logInfo(
            s"JEV_UNDERSTANDING_USAGE scenario=${scenario.name} questions=${questions.size} " +
              s"inputTokens=${result.usage.inputTokens} outputTokens=${result.usage.outputTokens}"
          )
          mismatchCount = evaluated.count(check => !check.isCorrect)
        yield assertTrue(mismatchCount == 0)
      }

    suite("Jev understanding diagnostics")(scenarioTests).provideShared(
      HttpClient.default,
    ) @@ ifEnvSet("RUN_JEV_UNDERSTANDING_DIAGNOSTICS") @@ ifEnvSet("TYPESAFE_API_KEY") @@
      withLiveSystem @@ withLiveClock @@ timeout(5.minutes) @@ sequential
