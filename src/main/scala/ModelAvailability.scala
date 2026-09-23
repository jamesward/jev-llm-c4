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

  final case class MantleModel(id: String) derives JsonDecoder
  final case class MantleModels(data: Vector[MantleModel]) derives JsonDecoder

  enum SelectionVariable(val name: String, val backend: BedrockModelCatalog.Backend):
    case ConverseModels extends SelectionVariable("CONVERSE_MODELS", BedrockModelCatalog.Backend.Converse)
    case MantleModels extends SelectionVariable("MANTLE_MODELS", BedrockModelCatalog.Backend.Mantle)

  final case class EnvironmentFilters(
    converse: Option[Set[String]],
    mantle: Option[Set[String]],
  ):
    def idsFor(backend: BedrockModelCatalog.Backend): Option[Set[String]] = backend match
      case BedrockModelCatalog.Backend.Converse => converse
      case BedrockModelCatalog.Backend.Mantle => mantle

  object EnvironmentFilters:
    def parse(converse: Option[String], mantle: Option[String]): EnvironmentFilters =
      EnvironmentFilters(parseList(converse), parseList(mantle))

    private def parseList(value: Option[String]): Option[Set[String]] =
      value.map(_.split(',').iterator.map(_.trim).filter(_.nonEmpty).toSet)

  enum RejectionReason:
    case UnknownModel
    case WrongBackend(actual: BedrockModelCatalog.Backend)
    case UnsupportedStructuredOutput
    case Unavailable(reason: String)

  final case class RejectedSelection(
    variable: SelectionVariable,
    modelId: String,
    reason: RejectionReason,
  ):
    def logMessage: String =
      val description = reason match
        case RejectionReason.UnknownModel => "not present in the coded model catalog"
        case RejectionReason.WrongBackend(actual) => s"belongs to backend=${actual.label}"
        case RejectionReason.UnsupportedStructuredOutput => "does not support native structured output on its configured backend"
        case RejectionReason.Unavailable(value) => s"not available for the configured API key: $value"
      s"Configured model selection rejected: variable=${variable.name}, modelId=$modelId, reason=$description"

  final case class Hidden(model: BedrockModelCatalog.Model, reason: String)

  final case class Snapshot(
    available: Vector[BedrockModelCatalog.Model],
    hidden: Vector[Hidden],
    warning: Option[String],
  )

  final case class Resolution(snapshot: Snapshot, rejected: Vector[RejectedSelection])

  def mantleModelIds(body: String): Either[String, Set[String]] =
    body.fromJson[MantleModels].map(_.data.map(_.id).toSet)

  def resolve(apiAvailable: Snapshot, filters: EnvironmentFilters): Resolution =
    val selectedCatalog = BedrockModelCatalog.all.filter: model =>
      filters.idsFor(model.backend).forall(_.contains(model.id))
    val selectedIds = selectedCatalog.map(_.id).toSet
    val available = apiAvailable.available.filter(model => selectedIds.contains(model.id))
    val hidden = apiAvailable.hidden.filter(value => selectedIds.contains(value.model.id))
    val rejected = SelectionVariable.values.toVector.flatMap: variable =>
      filters.idsFor(variable.backend).toVector.flatMap: ids =>
        ids.toVector.sorted.flatMap: id =>
          BedrockModelCatalog.findCoded(id) match
            case None => Some(RejectedSelection(variable, id, RejectionReason.UnknownModel))
            case Some(model) if model.backend != variable.backend =>
              Some(RejectedSelection(variable, id, RejectionReason.WrongBackend(model.backend)))
            case Some(model) if model.structuredOutput == BedrockModelCatalog.StructuredOutput.Unsupported =>
              Some(RejectedSelection(variable, id, RejectionReason.UnsupportedStructuredOutput))
            case Some(model) if !apiAvailable.available.exists(_.id == model.id) =>
              val reason = apiAvailable.hidden.find(_.model.id == model.id).map(_.reason).getOrElse("not returned by availability checks")
              Some(RejectedSelection(variable, id, RejectionReason.Unavailable(reason)))
            case Some(_) => None
    Resolution(
      Snapshot(
        available = available,
        hidden = hidden,
        warning =
          if selectedCatalog.isEmpty then Some("No coded models were selected by CONVERSE_MODELS or MANTLE_MODELS.")
          else if available.isEmpty then Some("None of the selected models are available for this account.")
          else None,
      ),
      rejected,
    )

  def codedCatalogLog: String =
    "Coded LLM model catalog:\n" + BedrockModelCatalog.coded.map(modelDescription).mkString("\n")

  def resolvedCatalogLog(models: Vector[BedrockModelCatalog.Model]): String =
    "Resolved user-visible LLM models:\n" +
      (if models.isEmpty then "(none)" else models.map(modelDescription).mkString("\n"))

  private def modelDescription(model: BedrockModelCatalog.Model): String =
    s"label=${model.label}, backend=${model.backend.label}, structuredOutput=${model.structuredOutput}, " +
      s"runtimeId=${model.id}, foundationId=${model.foundationModelId}"

  def evictionLog(hidden: Hidden): String =
    s"Bedrock model evicted from startup catalog after availability failure: " +
      s"label=${hidden.model.label}, backend=${hidden.model.backend.label}, " +
      s"runtimeId=${hidden.model.id}, foundationId=${hidden.model.foundationModelId}, reason=${hidden.reason}"

  val allCatalog: ULayer[ModelAvailability] =
    ZLayer.succeed(Static(Snapshot(BedrockModelCatalog.all, Vector.empty, None)))

  val live: URLayer[Client, ModelAvailability] =
    ZLayer.fromZIO:
      for
        client <- ZIO.service[Client]
        _ <- ZIO.logInfo(codedCatalogLog)
        apiKey <- ZIO.systemWith(_.env("AWS_BEARER_TOKEN_BEDROCK")).orDie
        converseSelection <- ZIO.systemWith(_.env(SelectionVariable.ConverseModels.name)).orDie
        mantleSelection <- ZIO.systemWith(_.env(SelectionVariable.MantleModels.name)).orDie
        filters = EnvironmentFilters.parse(converseSelection, mantleSelection)
        apiResult <- apiKey.filter(_.nonEmpty) match
          case None =>
            ZIO.succeed(Snapshot(
              available = Vector.empty,
              hidden = BedrockModelCatalog.all.map(Hidden(_, "Bedrock API key is not configured")),
              warning = Some("Configure the LLM provider API key to check and enable models."),
            ))
          case Some(key) => checkAll(client, key)
        resolution = resolve(apiResult, filters)
        _ <- ZIO.foreachDiscard(resolution.rejected)(rejected => ZIO.logWarning(rejected.logMessage))
        _ <- ZIO.foreachDiscard(resolution.snapshot.hidden)(hidden => ZIO.logWarning(evictionLog(hidden)))
        _ <- ZIO.logInfo(resolvedCatalogLog(resolution.snapshot.available))
        _ <- ZIO.logInfo(
          s"LLM startup availability: ${resolution.snapshot.available.size}/${BedrockModelCatalog.all.size} " +
            "native-structured-output models enabled"
        )
      yield Static(resolution.snapshot)

  private final case class Static(snapshot: Snapshot) extends ModelAvailability

  private def checkAll(client: Client, apiKey: String): UIO[Snapshot] =
    val (mantleCatalog, converseCatalog) = BedrockModelCatalog.all.partition:
      _.backend == BedrockModelCatalog.Backend.Mantle
    for
      converseChecked <- ZIO.foreachPar(converseCatalog)(checkConverse(client, apiKey, _))
      mantleChecked <- checkMantle(client, apiKey, mantleCatalog)
    yield
      val checkedById: Map[String, Either[Hidden, BedrockModelCatalog.Model]] =
        (converseChecked ++ mantleChecked).map:
          case result @ Right(model) => model.id -> result
          case result @ Left(hidden) => hidden.model.id -> result
        .toMap
      val checked = BedrockModelCatalog.all.map(model => checkedById(model.id))
      val available = checked.collect { case Right(model) => model }
      val hidden = checked.collect { case Left(value) => value }
      Snapshot(
        available = available,
        hidden = hidden,
        warning =
          if available.isEmpty then Some("No catalog models are available for this account.")
          else Option.when(hidden.nonEmpty)(s"${hidden.size} catalog model(s) were hidden because this account cannot use them."),
      )

  private def checkMantle(
    client: Client,
    apiKey: String,
    models: Vector[BedrockModelCatalog.Model],
  ): UIO[Vector[Either[Hidden, BedrockModelCatalog.Model]]] =
    if models.isEmpty then ZIO.succeed(Vector.empty)
    else
      val base = URL.decode(s"https://bedrock-mantle.${BedrockModelCatalog.RegionCode}.api.aws").toOption.get
      val authed = client.url(base).addHeader(Header.Authorization.Bearer(apiKey))
      ZIO.scoped:
        authed.get("/v1/models")
          .timeoutFail(RuntimeException("Mantle model-list check timed out"))(10.seconds)
          .flatMap: response =>
            response.body.asString.flatMap: body =>
              if response.status.isSuccess then
                ZIO.fromEither(mantleModelIds(body)).mapError(RuntimeException(_)).map: availableIds =>
                  models.map: model =>
                    if availableIds.contains(model.id) then Right(model)
                    else Left(Hidden(model, "model is not listed by the Mantle /v1/models endpoint"))
              else ZIO.succeed(hiddenAll(models, s"Mantle model-list API returned ${response.status.code}: ${body.take(300)}"))
      .catchAllCause: cause =>
        ZIO.succeed(hiddenAll(models, s"Mantle model-list check failed: ${cause.prettyPrint.take(500)}"))

  private def hiddenAll(
    models: Vector[BedrockModelCatalog.Model],
    reason: String,
  ): Vector[Either[Hidden, BedrockModelCatalog.Model]] =
    models.map(model => Left(Hidden(model, reason)))

  private def checkConverse(
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
