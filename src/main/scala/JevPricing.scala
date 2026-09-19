object JevPricing:
  val ModelId = "jev-latest"
  val InputUsdPerMillion = BigDecimal("0.042")
  val OutputUsdPerMillion = BigDecimal("0.00")
  val PricingVerifiedOn = "2026-09-18"
  val PricingSource = "https://docs.typesafe.ai/models"

  val pricing = BedrockModelCatalog.TokenPricing(
    inputUsdPerMillion = InputUsdPerMillion,
    outputUsdPerMillion = OutputUsdPerMillion,
  )
