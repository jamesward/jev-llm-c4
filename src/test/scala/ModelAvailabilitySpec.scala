import zio.test.*

object ModelAvailabilitySpec extends ZIOSpecDefault:
  private val available = ModelAvailability.Response(
    agreementAvailability = ModelAvailability.AgreementAvailability("AVAILABLE"),
    authorizationStatus = "AUTHORIZED",
    entitlementAvailability = "AVAILABLE",
    modelId = BedrockModelCatalog.Llama4Maverick.foundationModelId,
    regionAvailability = "AVAILABLE",
  )

  def spec = suite("ModelAvailability")(
    test("requires every availability signal") {
      assertTrue(
        available.isAvailable,
        !available.copy(agreementAvailability = ModelAvailability.AgreementAvailability("PENDING")).isAvailable,
        !available.copy(authorizationStatus = "NOT_AUTHORIZED").isAvailable,
        !available.copy(entitlementAvailability = "NOT_AVAILABLE").isAvailable,
        !available.copy(regionAvailability = "NOT_AVAILABLE").isAvailable,
      )
    },
    test("describes unavailable signals") {
      val response = available.copy(
        agreementAvailability = ModelAvailability.AgreementAvailability("ERROR", Some("agreement failed")),
        authorizationStatus = "NOT_AUTHORIZED",
      )
      assertTrue(
        response.unavailableReason.contains("agreement=ERROR"),
        response.unavailableReason.contains("authorization=NOT_AUTHORIZED"),
        response.unavailableReason.contains("agreement failed"),
      )
    },
    test("logs complete eviction identity and reason") {
      val model = BedrockModelCatalog.ClaudeOpus5
      val message = ModelAvailability.evictionLog(ModelAvailability.Hidden(model, "authorization=NOT_AUTHORIZED"))
      assertTrue(
        message.contains("evicted from startup catalog"),
        message.contains(model.label),
        message.contains(model.id),
        message.contains(model.foundationModelId),
        message.contains("authorization=NOT_AUTHORIZED"),
      )
    },
  )
