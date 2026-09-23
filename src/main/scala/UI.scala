import webjars.generated.WebJars as Gen
import zio.http.template2.*

object UI:
  private val tailwindUrl = Gen.url(Gen.Artifact.`tailwindcss__browser`, "dist/index.global.js")

  private val githubIcon: Dom =
    Dom.element("svg")(
      Dom.attr("xmlns", "http://www.w3.org/2000/svg"),
      Dom.attr("viewBox", "0 0 16 16"),
      Dom.attr("fill", "currentColor"),
      Dom.attr("aria-hidden", "true"),
      `class` := "h-5 w-5",
      Dom.raw("""<path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"/>"""),
    )

  private def emptySlot: Dom =
    div(
      `class` := "slot aspect-square rounded-full p-[7%]",
      div(`class` := "piece-empty h-full w-full rounded-full"),
    )

  private def playerCard(player: String, pieceClass: String, cardClass: String): Dom =
    val key = player.toLowerCase
    val costClass = if key == "jev" then "text-amber-200" else "text-rose-200"
    div(
      id := s"$key-card",
      `class` := cardClass,
      div(
        `class` := "flex items-center justify-between gap-3",
        div(
          `class` := "flex items-center gap-2",
          span(`class` := s"h-4 w-4 rounded-full $pieceClass"),
          strong(player),
        ),
        p(
          id := s"$key-cost",
          `class` := s"font-mono text-sm font-bold $costClass",
          "$0.000000",
        ),
      ),
      p(
        id := s"$key-time",
        `class` := "mt-1 text-xs text-slate-400",
        "0 turns · 0.0s",
      ),
      p(
        id := s"$key-usage",
        `class` := "mt-1 text-[10px] text-slate-500",
        "0 input · 0 output tokens",
      ),
    )

  private def moveDetailsModal: Dom =
    div(
      id := "move-details-modal",
      role := "dialog",
      Dom.attr("aria-modal", "true"),
      `class` := "fixed inset-0 z-50 hidden items-center justify-center bg-black/75 p-4",
      div(
        `class` := "max-h-[90vh] w-full max-w-4xl overflow-hidden rounded-2xl border border-white/10 bg-slate-900 shadow-2xl",
        div(
          `class` := "flex items-center justify-between border-b border-white/10 px-4 py-3",
          h2(id := "move-details-title", `class` := "font-bold text-white", "Turn details"),
          button(
            id := "move-details-close",
            `type` := "button",
            `class` := "rounded-lg border border-white/10 px-3 py-1.5 text-xs text-slate-300 hover:bg-white/5",
            "Close",
          ),
        ),
        div(
          `class` := "grid max-h-[calc(90vh-4rem)] gap-4 overflow-y-auto p-4 lg:grid-cols-2",
          div(
            `class` := "min-w-0",
            h3(`class` := "mb-2 text-xs font-bold uppercase tracking-wider text-slate-400", "Request"),
            pre(
              id := "move-details-request",
              `class` := "whitespace-pre-wrap break-words max-w-full overflow-x-auto rounded-xl bg-black/25 p-3 text-xs text-slate-300",
              "No request details recorded.",
            ),
          ),
          div(
            `class` := "min-w-0",
            h3(`class` := "mb-2 text-xs font-bold uppercase tracking-wider text-slate-400", "Response"),
            pre(
              id := "move-details-response",
              `class` := "whitespace-pre-wrap break-words max-w-full overflow-x-auto rounded-xl bg-black/25 p-3 text-xs text-slate-300",
              "No response details recorded.",
            ),
          ),
        ),
      ),
    )

  private def modelOption(model: BedrockModelCatalog.Model): Dom =
    option(
      value := model.id,
      s"${model.label} [${model.backend.label}] — $$${model.pricing.inputUsdPerMillion}/$$${model.pricing.outputUsdPerMillion} per 1M in/out",
    )

  def index(
    models: Vector[BedrockModelCatalog.Model],
    availabilityNotice: Option[String],
  ): Dom =
    html(
      lang := "en",
      `class` := "bg-slate-950",
      head(
        meta(charset := "UTF-8"),
        meta(name := "viewport", Dom.attr("content", "width=device-width, initial-scale=1.0")),
        title("Jev vs LLM · Connect Four"),
        script(src := tailwindUrl),
        Dom.element("style")(
          Dom.attr("type", "text/tailwindcss"),
          Dom.raw(UIAssets.tailwindCss),
        ),
      ),
      body(
        `class` := "min-h-screen text-slate-100 antialiased selection:bg-indigo-400/30",
        main(
          `class` := "mx-auto max-w-7xl px-4 py-7 sm:px-6 lg:px-8",
          header(
            `class` := "mb-7 flex flex-col gap-3 sm:flex-row sm:items-end sm:justify-between",
            div(
              div(
                `class` := "mb-2 inline-flex items-center gap-2 rounded-full border border-indigo-400/20 bg-indigo-400/10 px-3 py-1 text-xs font-semibold uppercase tracking-[.2em] text-indigo-200",
                "AI arena",
              ),
              h1(
                `class` := "text-3xl font-black tracking-tight sm:text-5xl",
                "Jev ", span(`class` := "text-slate-500", "vs"), " LLM",
              ),
              p(
                `class` := "mt-2 text-sm text-slate-400 sm:text-base",
                "Connect Four decisions in real time, with every LLM turn timed and priced.",
              ),
            ),
            div(
              `class` := "flex items-center gap-3",
              a(
                href := "https://github.com/jamesward/jev-llm-c4",
                Dom.attr("target", "_blank"),
                Dom.attr("rel", "noopener noreferrer"),
                Dom.attr("aria-label", "View source on GitHub"),
                Dom.attr("title", "View source on GitHub"),
                `class` := "rounded-lg p-2 text-slate-500 transition hover:bg-white/5 hover:text-white focus:outline-none focus:ring-2 focus:ring-indigo-400",
                githubIcon,
              ),
              div(
                id := "connection",
                `class` := "flex items-center gap-2 text-xs font-medium text-slate-500",
                span(`class` := "h-2 w-2 rounded-full bg-slate-600"), " Ready",
              ),
            ),
          ),
          section(
            `class` := "grid gap-6 lg:grid-cols-[minmax(0,1fr)_22rem]",
            div(
              `class` := "space-y-5",
              div(
                `class` := "rounded-2xl border border-white/10 bg-white/[.055] p-4 shadow-2xl backdrop-blur sm:p-5",
                form(
                  id := "game-form",
                  `class` := "grid gap-4 md:grid-cols-[minmax(0,1fr)_11rem_auto] md:items-end",
                  label(
                    `class` := "block text-sm font-semibold text-slate-300",
                    "Bedrock model · verified standard pricing",
                    select(
                      id := "model",
                      `class` := "mt-2 w-full rounded-xl border border-white/10 bg-slate-900 px-3 py-2.5 text-sm text-white outline-none ring-indigo-400 transition focus:ring-2 disabled:opacity-50",
                      Option.when(models.isEmpty)(Dom.attr("disabled", "disabled")).getOrElse(Dom.empty),
                      models.map(modelOption),
                    ),
                  ),
                  label(
                    `class` := "block text-sm font-semibold text-slate-300",
                    "Opening player",
                    select(
                      id := "first-player",
                      `class` := "mt-2 w-full rounded-xl border border-white/10 bg-slate-900 px-3 py-2.5 text-sm text-white outline-none ring-indigo-400 focus:ring-2",
                      option(value := "jev", "Jev"), option(value := "llm", "LLM"),
                    ),
                  ),
                  button(
                    id := "start",
                    Option.when(models.isEmpty)(Dom.attr("disabled", "disabled")).getOrElse(Dom.empty),
                    `class` := "rounded-xl bg-indigo-500 px-5 py-2.5 font-bold text-white shadow-lg shadow-indigo-900/40 transition hover:bg-indigo-400 disabled:cursor-not-allowed disabled:opacity-50",
                    "Start battle",
                  ),
                ),
                availabilityNotice.map: message =>
                  div(
                    `class` := "mt-3 rounded-lg border border-amber-400/25 bg-amber-400/10 px-3 py-2 text-xs text-amber-200",
                    message,
                  ),
                p(
                  `class` := "mt-2 text-[11px] text-slate-500",
                  s"LLM rates verified ${BedrockModelCatalog.PricingVerifiedOn}; Jev is $$${JevPricing.InputUsdPerMillion}/1M input (output free), verified ${JevPricing.PricingVerifiedOn}. Estimates use reported tokens.",
                ),
                p(
                  `class` := "mt-1 text-[11px] text-slate-500",
                  "Completed model and opening-player matchups are cached, then replayed with their original turn timing to avoid repeated inference costs.",
                ),
                div(
                  id := "error",
                  `class` := "mt-3 hidden rounded-lg border border-rose-400/30 bg-rose-400/10 px-3 py-2 text-sm text-rose-200",
                ),
              ),
              div(
                `class` := "rounded-3xl border border-white/10 bg-black/20 p-3 shadow-2xl sm:p-6",
                div(
                  `class` := "mb-4 flex items-center justify-between gap-3",
                  div(
                    p(`class` := "text-xs font-bold uppercase tracking-[.16em] text-slate-500", "Match status"),
                    h2(id := "status", `class` := "mt-1 text-lg font-bold text-white", "Choose a model to begin"),
                  ),
                  div(
                    `class` := "flex items-center gap-2",
                    div(
                      id := "turn-clock",
                      `class` := "hidden rounded-xl border border-indigo-400/20 bg-indigo-400/10 px-3 py-2 font-mono text-sm font-bold text-indigo-200",
                      "0.0s",
                    ),
                    button(
                      id := "cancel", `type` := "button",
                      `class` := "hidden rounded-lg border border-white/10 px-3 py-2 text-xs font-semibold text-slate-400 hover:bg-white/5 hover:text-white",
                      "Cancel",
                    ),
                  ),
                ),
                div(
                  `class` := "mx-auto max-w-2xl",
                  div(
                    id := "column-labels",
                    `class` := "mb-2 grid grid-cols-7 gap-1 px-2 text-center font-mono text-xs text-slate-600 sm:gap-2",
                    (0 until 7).map(value => span(value.toString)),
                  ),
                  div(
                    id := "board",
                    `class` := "board grid aspect-[7/6] grid-cols-7 gap-1 rounded-2xl border border-blue-300/20 bg-gradient-to-b from-blue-500 to-blue-800 p-2 sm:gap-2 sm:p-3",
                    Vector.fill(42)(emptySlot),
                  ),
                ),
                div(
                  `class` := "mt-5 grid grid-cols-2 gap-3",
                  playerCard("Jev", "piece-jev", "rounded-xl border border-amber-300/10 bg-amber-300/5 p-3 transition"),
                  playerCard("LLM", "piece-llm", "rounded-xl border border-rose-300/10 bg-rose-300/5 p-3 transition"),
                ),
              ),
            ),
            aside(
              `class` := "rounded-2xl border border-white/10 bg-white/[.045] p-4 shadow-xl backdrop-blur lg:max-h-[47rem]",
              div(
                `class` := "flex items-center justify-between",
                h2(`class` := "font-bold", "Turn history"),
                span(id := "move-count", `class` := "rounded-full bg-white/5 px-2 py-1 text-xs text-slate-400", "0 moves"),
              ),
              p(id := "model-label", `class` := "mt-1 truncate text-xs text-slate-400", "No model selected"),
              p(id := "model-pricing", `class` := "mt-1 text-[10px] text-slate-600", "Select a fixed-price model"),
              ol(
                id := "history",
                `class` := "mt-4 space-y-2 overflow-y-auto lg:max-h-[40rem]",
                li(
                  `class` := "rounded-xl border border-dashed border-white/10 p-4 text-center text-sm text-slate-600",
                  "Moves will appear here",
                ),
              ),
            ),
          ),
        ),
        moveDetailsModal,
        script.inlineJs(UIAssets.clientScript),
      ),
    )
