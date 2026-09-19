import zio.*

object RequiredEnvironment:
  enum Variable(val name: String):
    case BedrockApiKey extends Variable("AWS_BEARER_TOKEN_BEDROCK")
    case TypeSafeApiKey extends Variable("TYPESAFE_API_KEY")

  final case class Missing(variables: List[Variable]) extends RuntimeException(
    s"Missing required environment variable(s): ${variables.map(_.name).mkString(", ")}"
  )

  def validate(values: Map[String, String]): Either[Missing, Unit] =
    val missing = Variable.values.filter: variable =>
      values.get(variable.name).forall(_.trim.isEmpty)
    Either.cond(missing.isEmpty, (), Missing(missing.toList))

  val validateLive: IO[Missing, Unit] =
    ZIO.foreach(Variable.values): variable =>
      ZIO.systemWith(_.env(variable.name)).orDie.map(variable -> _)
    .flatMap: entries =>
      val values = entries.collect { case (variable, Some(value)) => variable.name -> value }.toMap
      ZIO.fromEither(validate(values))
