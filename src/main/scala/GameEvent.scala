import ConnectFour.*
import zio.*
import zio.json.*
import zio.stream.*

enum GameEventType derives JsonCodec:
  case TurnStart, TurnEnd

  def sseName: String = this match
    case TurnStart => "turn-start"
    case TurnEnd   => "turn-end"

final case class GameEvent(
  sequence: Long,
  eventType: GameEventType,
  game: GameSnapshot,
) derives JsonCodec

final case class GameEventChannel private (
  history: Ref[Vector[GameEvent]],
  hub: Hub[GameEvent],
  semaphore: Semaphore,
):
  def publish(eventType: GameEventType, game: GameSnapshot): UIO[GameEvent] =
    semaphore.withPermit(publishUnlocked(eventType, game))

  def transition(
    eventType: GameEventType,
    update: UIO[Option[GameSnapshot]],
  )(
    beforePublish: GameSnapshot => UIO[Unit],
  ): UIO[Option[GameSnapshot]] =
    semaphore.withPermit:
      update.flatMap:
        case value @ Some(game) => beforePublish(game) *> publishUnlocked(eventType, game).as(value)
        case None => ZIO.none

  def events: UIO[Vector[GameEvent]] = history.get

  def stream(afterSequence: Long = 0L): ZStream[Any, Nothing, GameEvent] =
    ZStream.unwrapScoped:
      for
        queue <- hub.subscribe
        allHistory <- history.get
        latestSequence = allHistory.lastOption.fold(0L)(_.sequence)
        effectiveAfter = math.min(math.max(0L, afterSequence), latestSequence)
        replay = allHistory.filter(_.sequence > effectiveAfter)
        lastSequence = latestSequence
        terminalAlreadySeen = allHistory.lastOption.exists(event =>
          event.sequence <= effectiveAfter && event.game.status != GameStatus.Thinking
        )
      yield
        if terminalAlreadySeen then ZStream.empty
        else
          (ZStream.fromIterable(replay) ++ ZStream.fromQueue(queue).filter(_.sequence > lastSequence))
            .takeUntil(event => event.game.status != GameStatus.Thinking)

  private def publishUnlocked(eventType: GameEventType, game: GameSnapshot): UIO[GameEvent] =
    history.modify: events =>
      val event = GameEvent(events.size.toLong + 1L, eventType, game)
      event -> (events :+ event)
    .flatMap(event => hub.publish(event).as(event))

object GameEventChannel:
  def make: UIO[GameEventChannel] =
    for
      history <- Ref.make(Vector.empty[GameEvent])
      hub <- Hub.unbounded[GameEvent]
      semaphore <- Semaphore.make(1)
    yield GameEventChannel(history, hub, semaphore)
