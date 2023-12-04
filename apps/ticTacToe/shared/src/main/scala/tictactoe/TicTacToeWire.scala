package tictactoe

import ujson.*
import scala.util.{Failure, Success, Try}

import cs214.webapp.wires.*
import cs214.webapp.exceptions.DecodingException
import cs214.webapp.{AppWire, WireFormat, UserId}

object TicTacToeWire extends AppWire[TicTacToeEvent, TicTacToeView]:
  import TicTacToeEvent.*
  import TicTacToeView.*
  import TicTacToeState.*
  import Board.*

  override object eventFormat extends WireFormat[TicTacToeEvent]:
    override def encode(t: TicTacToeEvent): Value =
      t match
        case Move(x, y) => PairWire(IntWire, IntWire).encode((x, y))

    override def decode(json: Value): Try[TicTacToeEvent] =
      Try:
        Move(IntWire.decode(json.arr(0)).get, IntWire.decode(json.arr(1)).get)

  override object viewFormat extends WireFormat[TicTacToeView]:

    def encode(t: TicTacToeView): Value =
      t match
        case Playing(board, yourTurn) => 
          val boardSeq = board.board.toSeq
          Obj(
            "playing" -> BooleanWire.encode(true),
            "board" -> SeqWire(OptionWire(StringWire)).encode(boardSeq),
            "yourturn" -> BooleanWire.encode(yourTurn)
          )
        case Finished(winner) => 
          Obj(
            "playing" -> BooleanWire.encode(false),
            "winner" -> OptionWire(StringWire).encode(winner)
          )

    def decode(json: Value): Try[TicTacToeView] =
      Try:
        if json("playing").bool then
          val board = Board(SeqWire(OptionWire(StringWire)).decode(json("board")).get.toVector)
          Playing(board, BooleanWire.decode(json("yourturn")).get)
        else
          Finished(OptionWire(StringWire).decode(json("winner")).get)
