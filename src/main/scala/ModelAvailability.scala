import zio.*
import zio.http.*
import zio.json.*

trait ModelAvailability:
  def snapshot: ModelAvailability.Snapshot
  def findAvailable(id: String): Option[BedrockModelCatalog.Model] = snapshot.available.find(_.id == id)

object ModelAvailability:
  final case class AgreementAvailability(
    status: String,
    errorMessage: Option[String] = None,
  ) derives JsonDecoder

  final case class Response(
    agreementAvailability: AgreementAvailability,
    authorizationStatus: String,
    entitlementAvailability: String,
    modelId: String,
    regionAvailability: String,
  ) derives JsonDecoder:
    def isAvailable: Boolean =
      agreementAvailability.status == "AVAILABLE" &&
        authorizationStatus == "AUTHORIZED" &&
        entitlementAvailability == "AVAILABLE" &&
        regionAvailability == "AVAILABLE"

    def unavailableReason: String =
      Vector(
        Option.when(agreementAvailability.status != "AVAILABLE")(s"agreement=${agreementAvailability.status}"),
        Option.when(authorizationStatus != "AUTHORIZED")(s"authorization=$authorizationStatus"),
        Option.when(entitlementAvailability != "AVAILABLE")(s"entitlement=$entitlementAvailability"),
        Option.when(regionAvailability != "AVAILABLE")(s"region=$regionAvailability"),
        agreementAvailability.errorMessage.filter(_.nonEmpty),
      ).flatten.mkString(", ")

  final case class Hidden(model: BedrockModelCatalog.Model, reason: String)

  final case class Snapshot(
    available: Vector[BedrockModelCatalog.Model],
    hidden: Vector[Hidden],
    warning: Option[String],
  )

  def evictionLog(hidden: Hidden): String =
    s"Bedrock model evicted from startup catalog after availability failure: " +
      s"label=${hidden.model.label}, runtimeId=${hidden.model.id}, " +
      s"foundationId=${hidden.model.foundationModelId}, reason=${hidden.reason}"

  val allCatalog: ULayer[ModelAvailability] =
    ZLayer.succeed(Static(Snapshot(BedrockModelCatalog.all, Vector.empty, None)))

  val live: URLayer[Client, ModelAvailability] =
    ZLayer.fromZIO:
      for
        client <- ZIO.service[Client]
        apiKey <- ZIO.systemWith(_.env("AWS_BEARER_TOKEN_BEDROCK")).orDie
        result <- apiKey.filter(_.nonEmpty) match
          case None =>
            ZIO.succeed(Snapshot(
              available = Vector.empty,
              hidden = BedrockModelCatalog.all.map(Hidden(_, "Bedrock API key is not configured")),
              warning = Some("Set AWS_BEARER_TOKEN_BEDROCK to check and enable models."),
            ))
          case Some(key) => checkAll(client, key)
        _ <- ZIO.foreachDiscard(result.hidden): hidden =>
          ZIO.logWarning(evictionLog(hidden))
        _ <- ZIO.logInfo(
          s"Bedrock startup availability: ${result.available.size}/${BedrockModelCatalog.all.size} catalog models enabled"
        )
      yield Static(result)

  private final case class Static(snapshot: Snapshot) extends ModelAvailability

  private def checkAll(client: Client, apiKey: String): UIO[Snapshot] =
    ZIO.foreachPar(BedrockModelCatalog.all)(check(client, apiKey, _)).map: checked =>
      val available = checked.collect { case Right(model) => model }
      val hidden = checked.collect { case Left(value) => value }
      Snapshot(
        available = available,
        hidden = hidden,
        warning =
          if available.isEmpty then Some("No catalog models are available to this Bedrock account.")
          else Option.when(hidden.nonEmpty)(s"${hidden.size} catalog model(s) were hidden because this account cannot use them."),
      )

  private def check(
    client: Client,
    apiKey: String,
    model: BedrockModelCatalog.Model,
  ): UIO[Either[Hidden, BedrockModelCatalog.Model]] =
    val base = URL.decode(s"https://bedrock.${BedrockModelCatalog.RegionCode}.amazonaws.com").toOption.get
    val authed = client.url(base).addHeader(Header.Authorization.Bearer(apiKey))
    ZIO.scoped:
      authed.get(s"/foundation-model-availability/${model.foundationModelId}")
        .timeoutFail(RuntimeException(s"availability check timed out for ${model.foundationModelId}"))(10.seconds)
        .flatMap: response =>
        response.body.asString.flatMap: body =>
          if response.status.isSuccess then
            ZIO.fromEither(body.fromJson[Response]).map: availability =>
              if availability.isAvailable then Right(model)
              else Left(Hidden(model, availability.unavailableReason))
          else ZIO.succeed(Left(Hidden(model, s"availability API returned ${response.status.code}: ${body.take(300)}")))
    .catchAllCause(cause => ZIO.succeed(Left(Hidden(model, s"availability check failed: ${cause.prettyPrint.take(500)}"))))
