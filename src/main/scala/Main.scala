import ConnectFour.*
import zio.*
import zio.http.*
import zio.json.*

object Main extends ZIOAppDefault:
  private def errorResponse(message: String, status: Status): Response =
    Response.json(ApiError(message).toJson).status(status)

  val routes: Routes[GameService & ModelAvailability, Nothing] = Routes(
    Method.GET / Root -> Handler.fromFunctionZIO[Request]: _ =>
      ZIO.serviceWith[ModelAvailability]: availability =>
        Response.html(UI.index(availability.snapshot.available, availability.snapshot.warning))
    ,
    Method.GET / "api" / "health" -> handler(Response.json("{\"status\":\"ok\"}")),
    Method.POST / "api" / "games" -> Handler.fromFunctionZIO[Request]: request =>
      (for
        body <- request.body.asString.mapError(error => s"Could not read request: ${error.getMessage}")
        start <- ZIO.fromEither(body.fromJson[StartGameRequest])
        game <- ZIO.serviceWithZIO[GameService](_.start(start))
      yield Response.json(game.toJson).status(Status.Created))
        .catchAll(error => ZIO.succeed(errorResponse(error, Status.BadRequest)))
    ,
    Method.GET / "api" / "games" / string("id") -> handler: (id: String, _: Request) =>
      ZIO.serviceWithZIO[GameService](_.get(id)).map:
        case Some(game) => Response.json(game.toJson)
        case None       => errorResponse("Game not found", Status.NotFound)
    ,
    Method.POST / "api" / "games" / string("id") / "cancel" -> handler: (id: String, _: Request) =>
      ZIO.serviceWithZIO[GameService](_.cancel(id)).map:
        case Some(game) => Response.json(game.toJson)
        case None       => errorResponse("Game not found", Status.NotFound)
    ,
  )

  private val serverLayer =
    ZLayer.fromZIO:
      for
        maybeHost <- ZIO.systemWith(_.env("HOST")).orDie
        maybePort <- ZIO.systemWith(_.env("PORT")).orDie
      yield Server.defaultWith(_.binding(
        maybeHost.filter(_.nonEmpty).getOrElse("127.0.0.1"),
        maybePort.flatMap(_.toIntOption).getOrElse(8080),
      ))
    .flatten

  def run =
    Server.serve(routes).provide(
      serverLayer,
      Client.default,
      ModelAvailability.live,
      MoveChooser.live,
      GameService.live,
    )
