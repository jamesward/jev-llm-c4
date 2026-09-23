import zio.test.*

object UISpec extends ZIOSpecDefault:
  private val rendered = UI.index(Vector(BedrockModelCatalog.Glm47Flash), None).render

  private def occurrences(value: String, fragment: String): Int =
    value.sliding(fragment.length).count(_ == fragment)

  def spec = suite("UI")(
    test("links to the GitHub repository with an accessible icon") {
      assertTrue(
        rendered.contains("href=\"https://github.com/jamesward/jev-llm-c4\""),
        rendered.contains("aria-label=\"View source on GitHub\""),
        rendered.contains("target=\"_blank\""),
        rendered.contains("rel=\"noopener noreferrer\""),
        rendered.contains("viewBox=\"0 0 16 16\""),
      )
    },
    test("mentions Bedrock only in the model selector") {
      assertTrue(
        occurrences(rendered, "Bedrock") == 1,
        rendered.contains("Bedrock model · verified standard pricing"),
        rendered.contains("every LLM turn timed and priced"),
        rendered.contains("LLM rates verified"),
        rendered.contains("matchups are cached"),
        UIAssets.clientScript.contains("Cached replay"),
        !rendered.contains("Bedrock cost"),
      )
    },
    test("renders one consolidated live summary per player below the board") {
      assertTrue(
        !rendered.contains("id=\"bedrock-cost\""),
        occurrences(rendered, "id=\"jev-cost\"") == 1,
        occurrences(rendered, "id=\"llm-cost\"") == 1,
        occurrences(rendered, "id=\"jev-time\"") == 1,
        occurrences(rendered, "id=\"llm-time\"") == 1,
        occurrences(rendered, "id=\"jev-usage\"") == 1,
        occurrences(rendered, "id=\"llm-usage\"") == 1,
        rendered.indexOf("id=\"board\"") < rendered.indexOf("id=\"jev-card\""),
        rendered.indexOf("id=\"board\"") < rendered.indexOf("id=\"llm-card\""),
      )
    },
    test("opens request and response details in a safe move-history modal") {
      val script = UIAssets.clientScript
      assertTrue(
        rendered.contains("id=\"move-details-modal\""),
        rendered.contains("id=\"move-details-request\""),
        rendered.contains("id=\"move-details-response\""),
        script.contains("data-move-turn"),
        script.contains("move.requestDetails"),
        script.contains("move.responseDetails"),
        script.contains("function formatJsonValue(value, depth)"),
        script.contains("containsOnlyPrimitives"),
        script.contains(".join(', ') + ']'"),
        script.contains("formatJsonValue(JSON.parse(value), 0)"),
        script.contains("move.player === 'Jev'"),
        script.contains("classList.toggle('whitespace-pre', isJev)"),
        script.contains("classList.toggle('whitespace-pre-wrap', !isJev)"),
        script.contains("classList.toggle('break-words', !isJev)"),
        script.contains("element.textContent"),
        occurrences(rendered, "overflow-x-auto") >= 2,
        occurrences(rendered, "min-w-0") >= 2,
        !script.contains("innerHTML = move.requestDetails"),
        !script.contains("innerHTML = move.responseDetails"),
      )
    },
    test("renders rejected turns in history without animating a board piece") {
      val script = UIAssets.clientScript
      assertTrue(
        script.contains("m.outcome === 'Rejected'"),
        script.contains("Rejected</span>"),
        script.contains("attempted column"),
        script.contains("no column returned"),
        script.contains("newest.outcome === 'Played'"),
        script.contains("move.outcome === 'Rejected' ? ' · rejected'"),
        script.contains("data-move-turn"),
      )
    },
    test("updates consolidated costs, timing, and tokens on every render") {
      val script = UIAssets.clientScript
      assertTrue(
        !script.contains("getElementById('bedrock-cost')"),
        script.contains("name + '-cost'"),
        script.contains("name + '-time'"),
        script.contains("name + '-usage'"),
        script.contains("sum + m.inputTokens"),
        script.contains("sum + m.outputTokens"),
        script.contains("next.bedrockCostUsd"),
        script.contains("next.jevCostUsd"),
        script.contains("new EventSource"),
        script.contains("/events"),
        script.contains("addEventListener('turn-start'"),
        script.contains("addEventListener('turn-end'"),
        script.contains("eventSource !== source"),
        script.contains("update.sequence <= lastEventSequence"),
        !script.contains("setInterval("),
        !script.contains("function poll("),
      )
    },
  )
