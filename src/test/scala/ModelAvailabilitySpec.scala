import zio.test.*

object ModelAvailabilitySpec extends ZIOSpecDefault:
  private val available = ModelAvailability.Response(
    agreementAvailability = ModelAvailability.AgreementAvailability("AVAILABLE"),
    authorizationStatus = "AUTHORIZED",
    entitlementAvailability = "AVAILABLE",
    modelId = BedrockModelCatalog.Glm47Flash.foundationModelId,
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
    test("decodes model IDs from the Mantle catalog") {
      val body = """{"object":"list","data":[{"id":"minimax.minimax-m2.5","object":"model"},{"id":"mistral.devstral-2-123b","object":"model"},{"id":"xai.grok-4.3","object":"model"}]}"""
      assertTrue(
        ModelAvailability.mantleModelIds(body) == Right(Set(
          BedrockModelCatalog.MiniMaxM25.id,
          BedrockModelCatalog.Devstral2123B.id,
          BedrockModelCatalog.Grok43.id,
        )),
        ModelAvailability.mantleModelIds("{}").isLeft,
      )
    },
    test("logs complete eviction identity and reason") {
      val model = BedrockModelCatalog.Glm47Flash
      val message = ModelAvailability.evictionLog(ModelAvailability.Hidden(model, "authorization=NOT_AUTHORIZED"))
      assertTrue(
        message.contains("evicted from startup catalog"),
        message.contains(model.label),
        message.contains(model.id),
        message.contains(model.foundationModelId),
        message.contains("authorization=NOT_AUTHORIZED"),
      )
    },
    test("unset filters retain every API-available model in catalog order") {
      val api = ModelAvailability.Snapshot(BedrockModelCatalog.all, Vector.empty, None)
      val resolution = ModelAvailability.resolve(api, ModelAvailability.EnvironmentFilters.parse(None, None))
      assertTrue(
        resolution.snapshot.available == BedrockModelCatalog.all,
        resolution.snapshot.hidden.isEmpty,
        resolution.rejected.isEmpty,
      )
    },
    test("explicit backend filters intersect with the selectable catalog") {
      val api = ModelAvailability.Snapshot(BedrockModelCatalog.all, Vector.empty, None)
      val filters = ModelAvailability.EnvironmentFilters.parse(
        Some(s" ${BedrockModelCatalog.Glm47Flash.id},${BedrockModelCatalog.Qwen332B.id},${BedrockModelCatalog.KimiK25.id},${BedrockModelCatalog.Glm47Flash.id} "),
        Some(s"${BedrockModelCatalog.Devstral2123B.id},${BedrockModelCatalog.Grok43.id}"),
      )
      val resolution = ModelAvailability.resolve(api, filters)
      assertTrue(
        resolution.snapshot.available == Vector(
          BedrockModelCatalog.Qwen332B,
          BedrockModelCatalog.Devstral2123B,
          BedrockModelCatalog.Grok43,
          BedrockModelCatalog.KimiK25,
          BedrockModelCatalog.Glm47Flash,
        ),
        resolution.rejected.isEmpty,
      )
    },
    test("unknown, wrong-backend, and unsupported selections are rejected") {
      val api = ModelAvailability.Snapshot(BedrockModelCatalog.all, Vector.empty, None)
      val filters = ModelAvailability.EnvironmentFilters(
        converse = Some(Set(
          "unknown.model",
          BedrockModelCatalog.MiniMaxM25.id,
          BedrockModelCatalog.NovaMicro.id,
        )),
        mantle = Some(Set(
          BedrockModelCatalog.Gpt56Mantle.id,
          BedrockModelCatalog.KimiK25.id,
        )),
      )
      val resolution = ModelAvailability.resolve(api, filters)
      assertTrue(
        resolution.snapshot.available.isEmpty,
        resolution.rejected.exists(value =>
          value.modelId == "unknown.model" && value.reason == ModelAvailability.RejectionReason.UnknownModel
        ),
        resolution.rejected.exists(value =>
          value.modelId == BedrockModelCatalog.MiniMaxM25.id &&
            value.reason == ModelAvailability.RejectionReason.WrongBackend(BedrockModelCatalog.Backend.Mantle)
        ),
        resolution.rejected.exists(value =>
          value.modelId == BedrockModelCatalog.KimiK25.id &&
            value.reason == ModelAvailability.RejectionReason.WrongBackend(BedrockModelCatalog.Backend.Converse)
        ),
        resolution.rejected.exists(value =>
          value.modelId == BedrockModelCatalog.NovaMicro.id &&
            value.reason == ModelAvailability.RejectionReason.UnsupportedStructuredOutput
        ),
        resolution.rejected.exists(value =>
          value.modelId == BedrockModelCatalog.Gpt56Mantle.id &&
            value.reason == ModelAvailability.RejectionReason.UnsupportedStructuredOutput
        ),
        resolution.rejected.find(_.modelId == BedrockModelCatalog.Gpt56Mantle.id)
          .exists(_.logMessage.contains("does not support native structured output")),
      )
    },
    test("explicit models unavailable to the API key stay out of the dropdown") {
      val model = BedrockModelCatalog.Glm47Flash
      val hidden = ModelAvailability.Hidden(model, "authorization=NOT_AUTHORIZED")
      val api = ModelAvailability.Snapshot(
        available = BedrockModelCatalog.all.filterNot(_.id == model.id),
        hidden = Vector(hidden),
        warning = None,
      )
      val filters = ModelAvailability.EnvironmentFilters(
        converse = Some(Set(model.id)),
        mantle = Some(Set.empty),
      )
      val resolution = ModelAvailability.resolve(api, filters)
      assertTrue(
        resolution.snapshot.available.isEmpty,
        resolution.snapshot.hidden == Vector(hidden),
        resolution.rejected.exists(value =>
          value.modelId == model.id &&
            value.reason == ModelAvailability.RejectionReason.Unavailable(hidden.reason)
        ),
      )
    },
    test("partially unavailable selections do not produce a UI warning") {
      val unavailable = BedrockModelCatalog.Glm47Flash
      val available = BedrockModelCatalog.Qwen332B
      val hidden = ModelAvailability.Hidden(unavailable, "authorization=NOT_AUTHORIZED")
      val api = ModelAvailability.Snapshot(Vector(available), Vector(hidden), None)
      val filters = ModelAvailability.EnvironmentFilters(
        converse = Some(Set(available.id, unavailable.id)),
        mantle = Some(Set.empty),
      )
      val resolution = ModelAvailability.resolve(api, filters)
      assertTrue(
        resolution.snapshot.available == Vector(available),
        resolution.snapshot.hidden == Vector(hidden),
        resolution.snapshot.warning.isEmpty,
      )
    },
    test("catalog logs include every coded capability and only selected models resolve") {
      val finalModels = Vector(BedrockModelCatalog.Qwen332B, BedrockModelCatalog.Devstral2123B)
      val coded = ModelAvailability.codedCatalogLog
      val resolved = ModelAvailability.resolvedCatalogLog(finalModels)
      assertTrue(
        BedrockModelCatalog.coded.forall(model => coded.contains(model.id)),
        coded.contains("structuredOutput=Native"),
        coded.contains("structuredOutput=Unsupported"),
        finalModels.forall(model => resolved.contains(model.id)),
        !resolved.contains(BedrockModelCatalog.NovaMicro.id),
        ModelAvailability.resolvedCatalogLog(Vector.empty).contains("(none)"),
      )
    },
  )
