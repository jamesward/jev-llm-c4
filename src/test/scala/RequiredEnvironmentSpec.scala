import zio.test.*

object RequiredEnvironmentSpec extends ZIOSpecDefault:
  private val bedrock = RequiredEnvironment.Variable.BedrockApiKey
  private val typeSafe = RequiredEnvironment.Variable.TypeSafeApiKey

  def spec = suite("RequiredEnvironment")(
    test("reports every missing variable") {
      val result = RequiredEnvironment.validate(Map.empty)
      assertTrue(
        result.left.exists(_.variables == List(bedrock, typeSafe)),
        result.left.exists(_.getMessage.contains("AWS_BEARER_TOKEN_BEDROCK")),
        result.left.exists(_.getMessage.contains("TYPESAFE_API_KEY")),
      )
    },
    test("treats blank values as missing") {
      val result = RequiredEnvironment.validate(Map(
        bedrock.name -> "  ",
        typeSafe.name -> "token",
      ))
      assertTrue(result.left.exists(_.variables == List(bedrock)))
    },
    test("accepts two nonblank values") {
      val result = RequiredEnvironment.validate(Map(
        bedrock.name -> "bedrock-token",
        typeSafe.name -> "typesafe-token",
      ))
      assertTrue(result == Right(()))
    },
  )
