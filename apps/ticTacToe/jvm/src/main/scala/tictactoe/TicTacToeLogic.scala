package tictactoe

import scala.collection.immutable.Queue
import scala.util.{Failure, Success, Try}

import cs214.webapp.*
import cs214.webapp.exceptions.*
import cs214.webapp.server.WebServer
import cs214.webapp.messages.Action

object TicTacToeStateMachine extends cs214.webapp.StateMachine[TicTacToeEvent, TicTacToeState, TicTacToeView]:
  import TicTacToeEvent.*
  import TicTacToeView.*
  import TicTacToeState.*
  import Board.*

  val name: String = "tictactoe"
  val wire = TicTacToeWire

  override def init(clients: Seq[UserId]): TicTacToeState =
    Play(Board(Vector.fill(9)(None)), 0, (clients(0), clients(1)))

  // Failures in the Try must hold instances of AppException
  // (from Exceptions.scala under lib/shared/)
  override def transition(state: TicTacToeState)(uid: UserId, event: TicTacToeEvent): Try[Seq[Action[TicTacToeState]]] =
    Try:
      state match
        case Play(board, turn, players) => 
          if uid == players(turn) then
            event match
              case Move(x, y) => 
                if x < 0 || 2 < x || y < 0 || 2 < y then throw IllegalMoveException("Out of bounds!")
                val newBoard = board.updated(x, y, uid)
                if newBoard.hasWinner() then
                  Seq(Action.Render(End(Some(uid))))
                else if newBoard.isFull() then
                  Seq(Action.Render(End(None)))
                else
                  val newTurn = if turn == 1 then 0 else 1
                  Seq(Action.Render(Play(newBoard, newTurn, players)))
          else
            throw NotYourTurnException()
        case End(winner) => 
          throw IllegalMoveException("Already over!")

  override def project(state: TicTacToeState)(uid: UserId): TicTacToeView =
    state match
      case Play(board, turn, players) => TicTacToeView.Playing(board, players(turn) == uid)
      case End(winner) => TicTacToeView.Finished(winner)

// Server registration magic
class register:
  WebServer.register(TicTacToeStateMachine)
