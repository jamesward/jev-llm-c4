import ConnectFour.*
import zio.*
import zio.http.Client
import zio.test.*
import zio.test.TestAspect.*

object JevMoveIntegrationSpec extends ZIOSpecDefault:
  private val partialBoard =
    List(0, 1, 2).foldLeft(Board.empty): (board, column) =>
      board.play(column, Player.Jev).toOption.get._1

  private val fullFirstColumn =
    List(
      0 -> Player.Llm,
      0 -> Player.Jev,
      0 -> Player.Llm,
      0 -> Player.Jev,
      0 -> Player.Jev,
      0 -> Player.Llm,
      1 -> Player.Llm,
    ).foldLeft(Board.empty):
      case (board, (column, player)) => board.play(column, player).toOption.get._1

  def spec = suite("simple Jev move integration")(
    test("returns one legal column Choice with usage and compact request details") {
      ZIO.serviceWithZIO[MoveChooser](_.choose(Player.Jev, partialBoard, "unused")).map: selection =>
        assertTrue(
          partialBoard.validColumns.contains(selection.column),
          selection.inputTokens > 0,
          selection.outputTokens > 0,
          selection.requestDetails.contains("boardRowsTopToBottom"),
          selection.requestDetails.contains("\"model\":\"jev-latest\""),
          selection.requestDetails.contains("\"questions\":{\"move\":"),
          !selection.requestDetails.contains("PostDropCandidate"),
          selection.responseDetails.contains("column_"),
          !selection.responseDetails.contains("probabilities"),
        )
    },
    test("does not expose or select a full column") {
      ZIO.serviceWithZIO[MoveChooser](_.choose(Player.Jev, fullFirstColumn, "unused")).map: selection =>
        assertTrue(
          selection.column != 0,
          fullFirstColumn.validColumns.contains(selection.column),
          !selection.requestDetails.contains("column_0"),
        )
    },
  ).provideShared(
    Client.default,
    MoveChooser.live,
  ) @@ ifEnvSet("TYPESAFE_API_KEY") @@ withLiveSystem @@ withLiveClock @@ timeout(1.minute) @@ sequential
