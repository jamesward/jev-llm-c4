import scala.math.BigDecimal.RoundingMode

object BedrockModelCatalog:
  val AwsPricingSource = "https://aws.amazon.com/bedrock/pricing/"
  val AwsPriceListSource = "https://pricing.us-east-1.amazonaws.com/offers/v1.0/aws/AmazonBedrock/current/index.json"
  val PricingVerifiedOn = "2026-09-18"
  val RegionCode = "us-east-1"

  final case class PriceSku(input: String, output: String)

  final case class TokenPricing(
    inputUsdPerMillion: BigDecimal,
    outputUsdPerMillion: BigDecimal,
  ):
    def estimateUsd(inputTokens: Int, outputTokens: Int): BigDecimal =
      val inputCost = BigDecimal(inputTokens) * inputUsdPerMillion
      val outputCost = BigDecimal(outputTokens) * outputUsdPerMillion
      ((inputCost + outputCost) / BigDecimal(1000000))
        .setScale(10, RoundingMode.HALF_UP)

  final case class Model(
    label: String,
    id: String,
    foundationModelId: String,
    pricing: TokenPricing,
    pricingBasis: String,
    pricingSource: String,
    modelCard: String,
    skus: Option[PriceSku] = None,
    maxOutputTokens: Int = 1024,
  )

  val Llama4Maverick: Model = Model(
    label = "Meta Llama 4 Maverick 17B",
    id = "us.meta.llama4-maverick-17b-instruct-v1:0",
    foundationModelId = "meta.llama4-maverick-17b-instruct-v1:0",
    pricing = TokenPricing(BigDecimal("0.24"), BigDecimal("0.97")),
    pricingBasis = "US geo standard",
    pricingSource = AwsPriceListSource,
    modelCard = "https://docs.aws.amazon.com/bedrock/latest/userguide/model-card-meta-llama-4-maverick-17b-instruct.html",
    skus = Some(PriceSku("JG52PDGZY6D7VPT9", "8BCTPZBR6YK9PVER")),
    maxOutputTokens = 256,
  )

  val Llama4Scout: Model = Model(
    label = "Meta Llama 4 Scout 17B",
    id = "us.meta.llama4-scout-17b-instruct-v1:0",
    foundationModelId = "meta.llama4-scout-17b-instruct-v1:0",
    pricing = TokenPricing(BigDecimal("0.17"), BigDecimal("0.66")),
    pricingBasis = "US geo standard",
    pricingSource = AwsPriceListSource,
    modelCard = "https://docs.aws.amazon.com/bedrock/latest/userguide/model-card-meta-llama-4-scout-17b-instruct.html",
    skus = Some(PriceSku("3NRMZNF7G8SFHU4N", "4MM3J6SFWQG77BS6")),
    maxOutputTokens = 256,
  )

  val ClaudeHaiku45: Model = Model(
    label = "Anthropic Claude Haiku 4.5",
    id = "global.anthropic.claude-haiku-4-5-20251001-v1:0",
    foundationModelId = "anthropic.claude-haiku-4-5-20251001-v1:0",
    pricing = TokenPricing(BigDecimal("1.00"), BigDecimal("5.00")),
    pricingBasis = "global standard",
    pricingSource = "https://www.anthropic.com/claude/haiku",
    modelCard = "https://docs.aws.amazon.com/bedrock/latest/userguide/model-card-anthropic-claude-haiku-4-5.html",
  )

  val NovaMicro: Model = Model(
    label = "Amazon Nova Micro",
    id = "us.amazon.nova-micro-v1:0",
    foundationModelId = "amazon.nova-micro-v1:0",
    pricing = TokenPricing(BigDecimal("0.035"), BigDecimal("0.14")),
    pricingBasis = "US geo standard",
    pricingSource = AwsPriceListSource,
    modelCard = "https://docs.aws.amazon.com/bedrock/latest/userguide/model-card-amazon-nova-micro.html",
    skus = Some(PriceSku("XAR69MSGZSU6FEM9", "YUSGHKDUPQRC75RK")),
    maxOutputTokens = 256,
  )

  val MistralLarge3: Model = Model(
    label = "Mistral Large 3",
    id = "mistral.mistral-large-3-675b-instruct",
    foundationModelId = "mistral.mistral-large-3-675b-instruct",
    pricing = TokenPricing(BigDecimal("0.50"), BigDecimal("1.50")),
    pricingBasis = "us-east-1 in-region standard",
    pricingSource = AwsPriceListSource,
    modelCard = "https://docs.aws.amazon.com/bedrock/latest/userguide/model-card-mistral-ai-mistral-large-3.html",
    skus = Some(PriceSku("F6SEZYUB98SAU4VQ", "3BG26FXFTP3HQ6SM")),
    maxOutputTokens = 256,
  )

  val DeepSeekV32: Model = Model(
    label = "DeepSeek V3.2",
    id = "deepseek.v3.2",
    foundationModelId = "deepseek.v3.2",
    pricing = TokenPricing(BigDecimal("0.62"), BigDecimal("1.85")),
    pricingBasis = "us-east-1 in-region standard",
    pricingSource = AwsPriceListSource,
    modelCard = "https://docs.aws.amazon.com/bedrock/latest/userguide/model-card-deepseek-deepseek-v3-2.html",
    skus = Some(PriceSku("GKSCNNJ6M7WBX7C3", "KXUENRNPAKP78CD6")),
  )

  val Qwen332B: Model = Model(
    label = "Qwen3 32B",
    id = "qwen.qwen3-32b-v1:0",
    foundationModelId = "qwen.qwen3-32b-v1:0",
    pricing = TokenPricing(BigDecimal("0.15"), BigDecimal("0.60")),
    pricingBasis = "us-east-1 in-region standard",
    pricingSource = AwsPriceListSource,
    modelCard = "https://docs.aws.amazon.com/bedrock/latest/userguide/model-card-qwen-qwen3-32b.html",
    skus = Some(PriceSku("5UQHJ7CFUZCUD9UX", "RA2QAWYCCM9R5BR6")),
  )

  val GptOss120B: Model = Model(
    label = "OpenAI gpt-oss-120b",
    id = "openai.gpt-oss-120b-1:0",
    foundationModelId = "openai.gpt-oss-120b-1:0",
    pricing = TokenPricing(BigDecimal("0.15"), BigDecimal("0.60")),
    pricingBasis = "us-east-1 in-region standard",
    pricingSource = AwsPriceListSource,
    modelCard = "https://docs.aws.amazon.com/bedrock/latest/userguide/model-card-openai-gpt-oss-120b.html",
    skus = Some(PriceSku("Q2U4FFKKTW34QVFG", "KGHQD8ZHB5468Z38")),
  )

  val ClaudeOpus5: Model = Model(
    label = "Anthropic Claude Opus 5",
    id = "global.anthropic.claude-opus-5",
    foundationModelId = "anthropic.claude-opus-5",
    pricing = TokenPricing(BigDecimal("5.00"), BigDecimal("25.00")),
    pricingBasis = "global standard",
    pricingSource = "https://platform.claude.com/docs/en/about-claude/pricing",
    modelCard = "https://docs.aws.amazon.com/bedrock/latest/userguide/model-card-anthropic-claude-opus-5.html",
  )

  val ClaudeSonnet5: Model = Model(
    label = "Anthropic Claude Sonnet 5",
    id = "global.anthropic.claude-sonnet-5",
    foundationModelId = "anthropic.claude-sonnet-5",
    pricing = TokenPricing(BigDecimal("2.00"), BigDecimal("10.00")),
    pricingBasis = "global standard",
    pricingSource = "https://www.anthropic.com/claude/sonnet",
    modelCard = "https://docs.aws.amazon.com/bedrock/latest/userguide/model-card-anthropic-claude-sonnet-5.html",
  )

  val Gpt56Sol: Model = Model(
    label = "OpenAI GPT-5.6 Sol",
    id = "global.openai.gpt-5.6-sol",
    foundationModelId = "openai.gpt-5.6-sol",
    pricing = TokenPricing(BigDecimal("4.00"), BigDecimal("20.00")),
    pricingBasis = "global standard, short context (≤272K input tokens)",
    pricingSource = "https://docs.aws.amazon.com/bedrock/latest/userguide/model-card-openai-gpt-56-sol.html",
    modelCard = "https://docs.aws.amazon.com/bedrock/latest/userguide/model-card-openai-gpt-56-sol.html",
  )

  val all: Vector[Model] = Vector(
    Llama4Maverick,
    Llama4Scout,
    ClaudeHaiku45,
    NovaMicro,
    MistralLarge3,
    DeepSeekV32,
    Qwen332B,
    GptOss120B,
    ClaudeOpus5,
    ClaudeSonnet5,
    Gpt56Sol,
  )

  def find(id: String): Option[Model] = all.find(_.id == id)
