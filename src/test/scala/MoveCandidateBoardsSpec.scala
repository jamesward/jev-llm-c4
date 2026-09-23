import com.jamesward.zio_typesafe_ai.TypeSafeAI
import ConnectFour.*
import zio.test.*

object MoveCandidateBoardsSpec extends ZIOSpecDefault:
  private val emptyRows = Vector.fill(5)(". . . . . . .")

  private def fullColumnZero: Board =
    (0 until ConnectFour.Rows).foldLeft(Board.empty): (board, turn) =>
      val player = if turn % 2 == 0 then Player.Jev else Player.Llm
      board.play(0, player).toOption.get._1

  def spec = suite("ASCII post-move candidate boards")(
    test("renders every Jev candidate in column order with coordinates") {
      val board = Board.empty
      val original = board.view
      val candidates = MoveCandidateBoards.from(board, Player.Jev)
      val expectedColumnThree =
        (Vector("After J is dropped into column 3, the board is:") ++
          emptyRows ++
          Vector(". . . J . . .", "0 1 2 3 4 5 6"))
          .mkString("\n")
      assertTrue(
        candidates.map(_.choiceId) == Vector.tabulate(ConnectFour.Columns)(column => s"column_$column"),
        candidates.map(_.column) == Vector.range(0, ConnectFour.Columns),
        candidates(3).description == expectedColumnThree,
        board.view == original,
      )
    },
    test("uses the requested player symbol") {
      val candidate = MoveCandidateBoards.from(Board.empty, Player.Llm)(3)
      assertTrue(
        candidate.description.contains("After L is dropped into column 3"),
        candidate.description.contains(". . . L . . ."),
        !candidate.description.contains(". . . J . . ."),
      )
    },
    test("describes a full column without inventing a resulting board") {
      val candidate = MoveCandidateBoards.from(fullColumnZero, Player.Jev).head
      assertTrue(
        candidate.choiceId == "column_0",
        candidate.description.contains("Column 0 cannot be taken"),
        !candidate.description.contains("the board is:"),
      )
    },
    test("renders every unlabeled opponent reply after each candidate") {
      val candidate = MoveCandidateBoards.withOpponentReplies(Board.empty, Player.Jev)(3)
      val replyHeaders = candidate.description.linesIterator.count(_.startsWith("After L replies in column "))
      assertTrue(
        candidate.description.contains("Possible L replies after this candidate:"),
        replyHeaders == ConnectFour.Columns,
        candidate.description.contains("After L replies in column 4, the board is:"),
        candidate.description.contains(". . . J L . ."),
        !candidate.description.toLowerCase.contains("winning"),
        !candidate.description.toLowerCase.contains("block"),
        !candidate.description.toLowerCase.contains("safe"),
      )
    },
    test("uses J as the opponent when generating L candidate replies") {
      val candidate = MoveCandidateBoards.withOpponentReplies(Board.empty, Player.Llm)(3)
      assertTrue(
        candidate.description.contains("After L is dropped into column 3"),
        candidate.description.contains("Possible J replies after this candidate:"),
        candidate.description.contains("After J replies in column 4"),
        candidate.description.contains(". . . L J . ."),
      )
    },
    test("Choice criteria contain the exact ASCII candidate descriptions") {
      val candidates = MoveCandidateBoards.from(Board.empty, Player.Jev)
      val criteria = TypeSafeAI.ChoiceCriteria(
        candidates.map(candidate => candidate.choiceId -> candidate.description)*
      ).toOption.get
      assertTrue(
        criteria.options.keySet == candidates.map(_.choiceId).toSet,
        criteria.options("column_3").as[String] == Right(candidates(3).description),
      )
    },
  )
