package memory

import scala.util.{Failure, Success, Try}

import cs214.webapp.*
import cs214.webapp.wires.*
import cs214.webapp.exceptions.DecodingException

object MemoryWire extends AppWire[MemoryEvent, MemoryView]:
  import MemoryEvent.*
  import MemoryView.*
  import ujson.*
  import StateView.*
  import PhaseView.*
  import CardView.*

  override object eventFormat extends WireFormat[MemoryEvent]:
    override def encode(event: MemoryEvent): Value =
      event match
        case Toggle(cardId) => Obj(
          "toggle" -> BooleanWire.encode(true),
          "cardId" -> IntWire.encode(cardId))
        case FlipSelected => Obj(
          "toggle" -> BooleanWire.encode(false)
        )

    override def decode(js: Value): Try[MemoryEvent] =
      Try:
        if js("toggle").bool then
          Toggle(IntWire.decode(js("cardId")).get)
        else
          FlipSelected

  override object viewFormat extends WireFormat[MemoryView]:

    override def encode(v: MemoryView): Value =
      v.stateView match
        case Playing(phase, currentPlayer, board) => 
          Obj(
            "playing" -> BooleanWire.encode(true), 
            "phase" -> IntWire.encode(phase.ordinal),
            "currentPlayer" -> StringWire.encode(currentPlayer),
            "board" -> SeqWire(cardFormat).encode(board),
            "alreadyMatched" -> MapWire(StringWire, SeqWire(StringWire)).encode(v.alreadyMatched)
            )
        case Finished(winnerIds) => 
          Obj(
            "playing" -> BooleanWire.encode(false),
            "winners" -> SetWire(StringWire).encode(winnerIds),
            "alreadyMatched" -> MapWire(StringWire, SeqWire(StringWire)).encode(v.alreadyMatched)
          )
      
    override def decode(js: Value): Try[MemoryView] =
      Try:
        if js("playing").bool then
          MemoryView(
            Playing(
              PhaseView.fromOrdinal(IntWire.decode(js("phase")).get),
              StringWire.decode(js("currentPlayer")).get,
              SeqWire(cardFormat).decode(js("board")).get
            ),
            MapWire(StringWire, SeqWire(StringWire)).decode(js("alreadyMatched")).get
          )
        else
          MemoryView(
            Finished(SetWire(StringWire).decode(js("winners")).get),
            MapWire(StringWire, SeqWire(StringWire)).decode(js("alreadyMatched")).get
          )
  
  object cardFormat extends WireFormat[CardView]:

    override def encode(t: CardView): Value = 
      t match
        case FaceUp(card) => Obj("view" -> IntWire.encode(t.ordinal), "card" -> StringWire.encode(card))
        case AlreadyMatched(card) => Obj("view" -> IntWire.encode(t.ordinal), "card" -> StringWire.encode(card))
        case _ => Obj("view" -> IntWire.encode(t.ordinal))

    override def decode(json: Value): Try[CardView] = 
      Try:
        IntWire.decode(json("view")).get match
          case 0 => FaceDown
          case 1 => Selected
          case 2 => FaceUp(StringWire.decode(json("card")).get)
          case 3 => AlreadyMatched(StringWire.decode(json("card")).get)
          case _ => throw DecodingException("cardView invalid")
