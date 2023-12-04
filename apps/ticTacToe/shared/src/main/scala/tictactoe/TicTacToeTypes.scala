
package tictactoe

import cs214.webapp.UserId

import scala.util.{Failure, Success, Try}
import cs214.webapp.exceptions.IllegalMoveException

/** Stores all information about the current game. */
enum TicTacToeState: // Change this type to hold actual information (use an enum, class, …)
  case Play(board: Board, turn: Int, players: (UserId, UserId))
  case End(winner: Option[UserId])

/** There is only one event in tic-tac-toe: clicking a cell. */
enum TicTacToeEvent:
  /** User clicked cell (x, y) */
  case Move(x: Int, y: Int)

/** Client views reflect the state of the game: playing or finished. */
enum TicTacToeView:
  /** Game in progress. */
  case Playing(board: Board, yourTurn: Boolean)

  /** Game over. [[winner]] is [[None]] if the game ended in a tie. */
  case Finished(winner: Option[UserId])

// Change this class definition to store board states.
case class Board(board: Vector[Option[UserId]]):
  /** Get the value in the cell at (r, c). */
  def apply(r: Int, c: Int): Option[UserId] =
    require(0 <= r && r < 3 && 0 <= c && c < 3) // Add an appropriate precondition
    board(3*r + c)

  def updated(r: Int, c: Int, userId: String): Board =
    if board(3*r + c).isDefined then throw IllegalMoveException("already occupied!")
    Board(board.updated(3*r + c, Some(userId)))

  def hasWinner(): Boolean =
    board(0).isDefined && board(0) == board(1) && board(1) == board(2) ||
    board(3).isDefined && board(3) == board(4) && board(4) == board(5) ||
    board(6).isDefined && board(6) == board(7) && board(7) == board(8) ||
    board(0).isDefined && board(0) == board(3) && board(3) == board(6) ||
    board(1).isDefined && board(1) == board(4) && board(4) == board(7) ||
    board(2).isDefined && board(2) == board(5) && board(5) == board(8) ||
    board(0).isDefined && board(0) == board(4) && board(4) == board(8) ||
    board(2).isDefined && board(2) == board(4) && board(4) == board(6)
  
  def isFull(): Boolean =
    board.forall(_.isDefined)