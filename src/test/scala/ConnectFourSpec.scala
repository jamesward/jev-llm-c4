import ConnectFour.*
import zio.json.*
import zio.test.*

object ConnectFourSpec extends ZIOSpecDefault:
  private def playAll(moves: List[(Int, Player)]): Either[String, Board] =
    moves.foldLeft[Either[String, Board]](Right(Board.empty)):
      case (board, (column, player)) => board.flatMap(_.play(column, player).map(_._1))

  def spec = suite("ConnectFour")(
    test("drops pieces to the lowest available row") {
      val result = for
        first <- Board.empty.play(3, Player.Jev)
        second <- first._1.play(3, Player.Llm)
      yield second
      assertTrue(
        result.toOption.exists(_._2 == 4),
        result.toOption.exists(_._1.view(5)(3) == "jev"),
        result.toOption.exists(_._1.view(4)(3) == "llm"),
      )
    },
    test("rejects out of range and full columns") {
      val full = playAll(List.fill(6)(0).zipWithIndex.map: (_, index) =>
        0 -> (if index % 2 == 0 then Player.Jev else Player.Llm)
      )
      assertTrue(
        Board.empty.play(-1, Player.Jev).isLeft,
        Board.empty.play(7, Player.Jev).isLeft,
        full.toOption.exists(_.play(0, Player.Jev).isLeft),
        full.toOption.exists(!_.validColumns.contains(0)),
      )
    },
    test("detects horizontal, vertical, and both diagonal wins") {
      val horizontal = playAll(List(0, 1, 2, 3).map(_ -> Player.Jev))
      val vertical = playAll(List.fill(4)(2 -> Player.Llm))
      val rising = playAll(List(
        0 -> Player.Jev,
        1 -> Player.Llm, 1 -> Player.Jev,
        2 -> Player.Llm, 2 -> Player.Llm, 2 -> Player.Jev,
        3 -> Player.Llm, 3 -> Player.Llm, 3 -> Player.Llm, 3 -> Player.Jev,
      ))
      val falling = playAll(List(
        3 -> Player.Jev,
        2 -> Player.Llm, 2 -> Player.Jev,
        1 -> Player.Llm, 1 -> Player.Llm, 1 -> Player.Jev,
        0 -> Player.Llm, 0 -> Player.Llm, 0 -> Player.Llm, 0 -> Player.Jev,
      ))
      assertTrue(
        horizontal.toOption.flatMap(_.winner).contains(Player.Jev),
        vertical.toOption.flatMap(_.winner).contains(Player.Llm),
        rising.toOption.flatMap(_.winner).contains(Player.Jev),
        falling.toOption.flatMap(_.winner).contains(Player.Jev),
      )
    },
    test("computes tactical facts for every legal column") {
      val opponentThreat = playAll(List(
        0 -> Player.Jev,
        1 -> Player.Jev,
        2 -> Player.Jev,
      )).toOption.get
      val defense = opponentThreat.tacticalOptions(Player.Llm)
      val block = defense.find(_.column == 3)
      val unsafe = defense.find(_.column == 4)

      val ownThreat = playAll(List.fill(3)(2 -> Player.Llm)).toOption.get
      val winning = ownThreat.tacticalOptions(Player.Llm).find(_.column == 2)

      assertTrue(
        defense.map(_.column) == opponentThreat.validColumns,
        block.exists(_.blocksImmediateThreat),
        block.exists(_.opponentWinningReplies.isEmpty),
        block.exists(_.landingRow == 5),
        unsafe.exists(_.opponentWinningReplies.contains(3)),
        winning.exists(_.winsNow),
        ownThreat.winningColumns(Player.Llm).contains(2),
      )
    },
    test("catalog contains the verified standard us-east-1 prices and SKUs") {
      val maverick = BedrockModelCatalog.Llama4Maverick
      val scout = BedrockModelCatalog.Llama4Scout
      assertTrue(
        BedrockModelCatalog.coded.size == 20,
        BedrockModelCatalog.coded.map(_.id).distinct.size == BedrockModelCatalog.coded.size,
        BedrockModelCatalog.all.size == 11,
        BedrockModelCatalog.all.forall(
          _.structuredOutput == BedrockModelCatalog.StructuredOutput.Native
        ),
        BedrockModelCatalog.all.filterNot(_ == BedrockModelCatalog.MiniMaxM25)
          .forall(_.maxOutputTokens <= 256),
        BedrockModelCatalog.MiniMaxM25.maxOutputTokens == 2048,
        BedrockModelCatalog.coded.filter(
          _.structuredOutput == BedrockModelCatalog.StructuredOutput.Unsupported
        ).toSet == Set(
          BedrockModelCatalog.Llama4Maverick,
          BedrockModelCatalog.Llama4Scout,
          BedrockModelCatalog.NovaMicro,
          BedrockModelCatalog.ClaudeOpus5,
          BedrockModelCatalog.ClaudeSonnet5,
          BedrockModelCatalog.Gpt56Sol,
          BedrockModelCatalog.ClaudeOpus5Mantle,
          BedrockModelCatalog.Gpt56Mantle,
          BedrockModelCatalog.ClaudeHaiku45Mantle,
        ),
        !BedrockModelCatalog.all.contains(BedrockModelCatalog.NovaMicro),
        BedrockModelCatalog.MiniMaxM25.backend == BedrockModelCatalog.Backend.Mantle,
        BedrockModelCatalog.MiniMaxM25.pricing == BedrockModelCatalog.TokenPricing(BigDecimal("0.30"), BigDecimal("1.20")),
        BedrockModelCatalog.Devstral2123B.backend == BedrockModelCatalog.Backend.Mantle,
        BedrockModelCatalog.Devstral2123B.pricing == BedrockModelCatalog.TokenPricing(BigDecimal("0.40"), BigDecimal("2.00")),
        BedrockModelCatalog.Grok43.id == "xai.grok-4.3",
        BedrockModelCatalog.Grok43.backend == BedrockModelCatalog.Backend.Mantle,
        BedrockModelCatalog.Grok43.structuredOutput == BedrockModelCatalog.StructuredOutput.Native,
        BedrockModelCatalog.Grok43.mantleEndpoint == BedrockModelCatalog.MantleEndpoint.OpenAI,
        BedrockModelCatalog.Grok43.pricing == BedrockModelCatalog.TokenPricing(BigDecimal("1.25"), BigDecimal("2.50")),
        BedrockModelCatalog.ClaudeOpus5Mantle.id == "anthropic.claude-opus-5",
        BedrockModelCatalog.Gpt56Mantle.id == "openai.gpt-5.6",
        BedrockModelCatalog.ClaudeHaiku45Mantle.id == "anthropic.claude-haiku-4-5",
        BedrockModelCatalog.KimiK25.id == "moonshotai.kimi-k2.5",
        BedrockModelCatalog.KimiK25.backend == BedrockModelCatalog.Backend.Converse,
        BedrockModelCatalog.KimiK25.structuredOutput == BedrockModelCatalog.StructuredOutput.Native,
        BedrockModelCatalog.KimiK25.pricing == BedrockModelCatalog.TokenPricing(BigDecimal("0.60"), BigDecimal("3.00")),
        BedrockModelCatalog.Glm47Flash.backend == BedrockModelCatalog.Backend.Converse,
        BedrockModelCatalog.Glm47Flash.pricing == BedrockModelCatalog.TokenPricing(BigDecimal("0.07"), BigDecimal("0.40")),
        BedrockModelCatalog.NemotronNano330B.backend == BedrockModelCatalog.Backend.Converse,
        BedrockModelCatalog.NemotronNano330B.pricing == BedrockModelCatalog.TokenPricing(BigDecimal("0.06"), BigDecimal("0.24")),
        maverick.pricing.inputUsdPerMillion == BigDecimal("0.24"),
        maverick.pricing.outputUsdPerMillion == BigDecimal("0.97"),
        maverick.skus.contains(BedrockModelCatalog.PriceSku("JG52PDGZY6D7VPT9", "8BCTPZBR6YK9PVER")),
        scout.pricing.inputUsdPerMillion == BigDecimal("0.17"),
        scout.pricing.outputUsdPerMillion == BigDecimal("0.66"),
        scout.skus.contains(BedrockModelCatalog.PriceSku("3NRMZNF7G8SFHU4N", "4MM3J6SFWQG77BS6")),
        scout.pricing.estimateUsd(1000, 100) == BigDecimal("0.0002360000"),
        BedrockModelCatalog.ClaudeHaiku45.pricing == BedrockModelCatalog.TokenPricing(BigDecimal("1.00"), BigDecimal("5.00")),
        BedrockModelCatalog.NovaMicro.pricing == BedrockModelCatalog.TokenPricing(BigDecimal("0.035"), BigDecimal("0.14")),
        BedrockModelCatalog.NovaMicro.skus.contains(BedrockModelCatalog.PriceSku("XAR69MSGZSU6FEM9", "YUSGHKDUPQRC75RK")),
        BedrockModelCatalog.MistralLarge3.pricing == BedrockModelCatalog.TokenPricing(BigDecimal("0.50"), BigDecimal("1.50")),
        BedrockModelCatalog.MistralLarge3.skus.contains(BedrockModelCatalog.PriceSku("F6SEZYUB98SAU4VQ", "3BG26FXFTP3HQ6SM")),
        BedrockModelCatalog.DeepSeekV32.pricing == BedrockModelCatalog.TokenPricing(BigDecimal("0.62"), BigDecimal("1.85")),
        BedrockModelCatalog.DeepSeekV32.skus.contains(BedrockModelCatalog.PriceSku("GKSCNNJ6M7WBX7C3", "KXUENRNPAKP78CD6")),
        BedrockModelCatalog.Qwen332B.pricing == BedrockModelCatalog.TokenPricing(BigDecimal("0.15"), BigDecimal("0.60")),
        BedrockModelCatalog.Qwen332B.skus.contains(BedrockModelCatalog.PriceSku("5UQHJ7CFUZCUD9UX", "RA2QAWYCCM9R5BR6")),
        BedrockModelCatalog.GptOss120B.pricing == BedrockModelCatalog.TokenPricing(BigDecimal("0.15"), BigDecimal("0.60")),
        BedrockModelCatalog.GptOss120B.skus.contains(BedrockModelCatalog.PriceSku("Q2U4FFKKTW34QVFG", "KGHQD8ZHB5468Z38")),
        BedrockModelCatalog.ClaudeOpus5.id == "global.anthropic.claude-opus-5",
        BedrockModelCatalog.ClaudeOpus5.pricing == BedrockModelCatalog.TokenPricing(BigDecimal("5.00"), BigDecimal("25.00")),
        BedrockModelCatalog.ClaudeSonnet5.id == "global.anthropic.claude-sonnet-5",
        BedrockModelCatalog.ClaudeSonnet5.pricing == BedrockModelCatalog.TokenPricing(BigDecimal("2.00"), BigDecimal("10.00")),
        BedrockModelCatalog.Gpt56Sol.id == "global.openai.gpt-5.6-sol",
        BedrockModelCatalog.Gpt56Sol.pricing == BedrockModelCatalog.TokenPricing(BigDecimal("4.00"), BigDecimal("20.00")),
        JevPricing.InputUsdPerMillion == BigDecimal("0.042"),
        JevPricing.OutputUsdPerMillion == BigDecimal("0.00"),
        JevPricing.pricing.estimateUsd(1000, 100) == BigDecimal("0.0000420000"),
      )
    },
    test("serializes pricing, cost, and rejected turns in a game snapshot") {
      val model = BedrockModelCatalog.Glm47Flash
      val rejected = MoveRecord(
        turn = 1,
        player = Player.Jev,
        column = Some(3),
        row = None,
        outcome = MoveOutcome.Rejected,
        durationMs = 25,
        note = "Jev selected illegal column 3",
        inputTokens = 10,
        outputTokens = 2,
        estimatedCostUsd = BigDecimal("0.000001"),
      )
      val snapshot = GameSnapshot(
        id = "game-1",
        modelId = model.id,
        modelLabel = model.label,
        inputUsdPerMillion = model.pricing.inputUsdPerMillion,
        outputUsdPerMillion = model.pricing.outputUsdPerMillion,
        bedrockCostUsd = BigDecimal("0.000123"),
        jevInputUsdPerMillion = JevPricing.InputUsdPerMillion,
        jevOutputUsdPerMillion = JevPricing.OutputUsdPerMillion,
        jevCostUsd = BigDecimal("0.000042"),
        board = Board.empty.view,
        status = GameStatus.Thinking,
        currentPlayer = Some(Player.Jev),
        winner = None,
        moves = Vector(rejected),
        message = "Jev is thinking",
        createdAtMs = 1L,
        turnStartedAtMs = Some(1L),
        finishedAtMs = None,
      )
      assertTrue(snapshot.toJson.fromJson[GameSnapshot] == Right(snapshot))
    },
  )
