import ConnectFour.*
import zio.*
import zio.http.*
import zio.json.*

object Main extends ZIOAppDefault:
  private def errorResponse(message: String, status: Status): Response =
    Response.json(ApiError(message).toJson).status(status)

  private val MaxStartGameRequestBytes = 1024L

  private def startGameRequest(request: Request): IO[String, StartGameRequest] =
    request.body.asStream
      .take(MaxStartGameRequestBytes + 1L)
      .runCollect
      .mapError(error => s"Could not read request: ${error.getMessage}")
      .flatMap: bytes =>
        if bytes.length > MaxStartGameRequestBytes then
          ZIO.fail(s"Game request exceeds $MaxStartGameRequestBytes bytes")
        else
          val body = new String(bytes.toArray, java.nio.charset.StandardCharsets.UTF_8)
          ZIO.fromEither(body.fromJson[StartGameRequest])

  def terminalEventAcknowledged(
    game: GameSnapshot,
    lastEventId: Option[Long],
    latest: GameEvent,
  ): Boolean =
    game.status != GameStatus.Thinking && lastEventId.exists(_ >= latest.sequence)

  val routes: Routes[GameService & ModelAvailability, Nothing] = Routes(
    Method.GET / Root -> Handler.fromFunctionZIO[Request]: _ =>
      ZIO.serviceWith[ModelAvailability]: availability =>
        Response.html(UI.index(availability.snapshot.available, availability.snapshot.warning))
    ,
    Method.GET / "api" / "health" -> handler(Response.json("{\"status\":\"ok\"}")),
    Method.POST / "api" / "games" -> Handler.fromFunctionZIO[Request]: request =>
      (for
        start <- startGameRequest(request)
        game <- ZIO.serviceWithZIO[GameService](_.start(start))
      yield Response.json(game.toJson).status(Status.Created))
        .catchAll(error => ZIO.succeed(errorResponse(error, Status.BadRequest)))
    ,
    Method.GET / "api" / "games" / string("id") -> handler: (id: String, _: Request) =>
      ZIO.serviceWithZIO[GameService](_.get(id)).map:
        case Some(game) => Response.json(game.toJson)
        case None       => errorResponse("Game not found", Status.NotFound)
    ,
    Method.GET / "api" / "games" / string("id") / "events" -> handler: (id: String, request: Request) =>
      val lastEventId = request.rawHeader("Last-Event-ID").flatMap(_.toLongOption)
      val afterSequence = lastEventId match
        case Some(value) => value
        case None => 0L
      ZIO.serviceWithZIO[GameService]: service =>
        service.get(id).flatMap:
          case None => ZIO.succeed(errorResponse("Game not found", Status.NotFound))
          case Some(game) =>
            service.lastEvent(id).flatMap:
              case Some(latest)
                  if terminalEventAcknowledged(game, lastEventId, latest) =>
                ZIO.succeed(Response.status(Status.NoContent))
              case _ =>
                service.events(id, afterSequence).map:
                  case Some(events) =>
                    Response.fromServerSentEvents(
                      events.map: event =>
                        ServerSentEvent(
                          data = event.toJson,
                          eventType = Some(event.eventType.sseName),
                          id = Some(event.sequence.toString),
                        )
                    ).addHeader(Header.Custom("Cache-Control", "no-cache"))
                  case None => errorResponse("Game not found", Status.NotFound)
    ,
    Method.POST / "api" / "games" / string("id") / "cancel" -> handler: (id: String, _: Request) =>
      ZIO.serviceWithZIO[GameService](_.cancel(id)).map:
        case Some(game) => Response.json(game.toJson)
        case None       => errorResponse("Game not found", Status.NotFound)
    ,
  )

  private val serverLayer =
    ZLayer.fromZIO:
      ZIO.systemWith(_.env("PORT")).map: maybePort =>
        maybePort.flatMap(_.toIntOption).fold(Server.default)(Server.defaultWithPort)
    .flatten

  def run =
    RequiredEnvironment.validateLive.flatMap: _ =>
      Server.serve(routes).provide(
        serverLayer,
        Client.default,
        ModelAvailability.live,
        MoveChooser.live,
        GameService.live,
      )
