package memory

import scala.util.{Try, Random}

import ujson.Value

import cs214.webapp.*
import cs214.webapp.messages.*
import cs214.webapp.exceptions.*
import cs214.webapp.server.WebServer

import memory.*
import MemoryState.*
import CardView.*
import PhaseView.*
import MemoryView.*
import StateView.*
import MemoryEvent.*

// Feel free to tweak this value!
private val SHOW_CARDS_PAUSE_MS = 2500

object MemoryStateMachine extends cs214.webapp.StateMachine[MemoryEvent, MemoryState, MemoryView]:

  val name: String = "memory"
  val wire = MemoryWire

  def Deck(cards: String): Vector[String] =
    cards.strip.split(" +").to(Vector)

  val DECKS: Map[String, Vector[String]] = Map(
    "Simple" -> Deck("""
      💫 ⭐️
    """),
    "Stars" -> Deck("""
      💫 ⭐️ 🌟 ✨ ☀️
    """),
    "Animals" -> Deck("""
      🐵 🐒 🦍 🦧 🐶 🐕 🦮 🐕‍🦺
      🐩 🐺 🦊 🦝 🐱 🐈 🐈‍⬛ 🦁
      🐯 🐅 🐆 🐴 🫎 🫏 🐎 🦄
      🦓 🦌 🦬 🐮 🐂 🐃 🐄 🐷
      🐖 🐗 🐽 🐏 🐑 🐐 🐪 🐫
      🦙 🦒 🐘 🦣 🦏 🦛 🐭 🐁
      🐀 🐹 🐰 🐇 🐿️ 🦫 🦔 🦇
      🐻 🐻‍❄️ 🐨 🐼 🦥 🦦 🦨 🦘
      🦡
    """),
    "Birds" -> Deck("""
      🦃 🐔 🐓 🐣 🐤 🐥 🐦 🐧
      🕊️ 🦅 🦆 🦢 🦉 🦤 🪶 🦩
      🦚 🦜 🪽 🐦‍⬛ 🪿
    """),
    "Marine & Reptiles" -> Deck("""
      🐸 🐊 🐢 🦎 🐍 🐲 🐉 🦕
      🦖 🐳 🐋 🐬 🦭 🐟 🐠 🐡
      🦈 🐙 🐚 🪸 🪼 🦀 🦞 🦐
      🦑 🦪
    """),
    "Bugs" -> Deck("""
      🐌 🦋 🐛 🐜 🐝 🪲 🐞 🦗
      🪳 🕷️ 🕸️ 🦂 🦟 🪰 🪱 🦠
    """),
    "Plants" -> Deck("""
      💐 🌸 💮 🪷 🏵️ 🌹 🥀 🌺
      🌻 🌼 🌷 🪻 🌱 🪴 🌲 🌳
      🌴 🌵 🌾 🌿 ☘️ 🍀 🍁 🍂
      🍃 🍄 🪨 🪵
    """)
  )

  // Use any strings you want here — the tests don't check for these specific emoji
  val CARDS: Vector[String] = DECKS("Bugs")

  /** Creates a new application state. */
  override def init(clients: Seq[UserId]): MemoryState =
    val board = Random.shuffle(CARDS.flatMap(s => List((s, FaceDown), (s, FaceDown))))
    val scores = clients.map(u => (u, Seq())).toMap
    Play(clients, board, SelectingCards, scores, List[Int]())

  override def transition(state: MemoryState)(userId: UserId, event: MemoryEvent): Try[Seq[Action[MemoryState]]] =
    Try:
      state match
        case Play(players, board, SelectingCards, alreadyMatched, selected) => 
          if players.head == userId then
            event match
              case Toggle(cardId) => 
                selected.headOption match
                  case None => 
                    if board(cardId)._2.ordinal == 3 then
                      throw IllegalMoveException("already matched!")
                    else
                      val entry = (board(cardId)._1, Selected)
                      Seq(Action.Render(Play(players, board.updated(cardId, entry), SelectingCards, alreadyMatched, cardId :: selected)))
                  case Some(card) => 
                    if cardId == card then
                      val entry = (board(cardId)._1, FaceDown)
                      Seq(Action.Render(Play(players, board.updated(cardId, entry), SelectingCards, alreadyMatched, selected.tail)))
                    else
                      if board(cardId)._2.ordinal == 3 then
                        throw IllegalMoveException("already matched!")
                      else
                        val entry = (board(cardId)._1, Selected)
                        Seq(Action.Render(Play(players, board.updated(cardId, entry), CardsSelected, alreadyMatched, cardId :: selected)))
              case FlipSelected => throw IllegalMoveException("not enough cards selected!")
          else
            throw NotYourTurnException()
        case Play(players, board, CardsSelected, alreadyMatched, selected) => 
          if players.head == userId then
            event match
              case Toggle(cardId) => 
                if selected.contains(cardId) then
                  val entry = (board(cardId)._1, FaceDown)
                  Seq(Action.Render(Play(players, board.updated(cardId, entry), SelectingCards, alreadyMatched, selected.filter(_ != cardId))))
                else
                  throw IllegalMoveException("select max 2 card")
              case FlipSelected => 
                val entry0 = (board(selected(0))._1, FaceUp(board(selected(0))._1))
                val entry1 = (board(selected(1))._1, FaceUp(board(selected(1))._1))
                if entry1._1 == entry0._1 then
                  val entry0matched = (board(selected(0))._1, AlreadyMatched(board(selected(0))._1))
                  val entry1matched = (board(selected(1))._1, AlreadyMatched(board(selected(1))._1))
                  val newBoard = board.updated(selected(0), entry0).updated(selected(1), entry1)
                  val newMatched = alreadyMatched.updated(userId, alreadyMatched(userId).appended(entry0._1).appended(entry1._1))
                  if newBoard.forall(s => s._2.ordinal == 3 || s._2.ordinal == 2) then
                    val maxCards = alreadyMatched.maxBy(_._2.length)._2.length
                    val winners = alreadyMatched.filter(_._2.length == maxCards).toSet.map(_._1)
                    Seq(Action.Render(Play(players, newBoard, GoodMatch, newMatched, List())), 
                    Action.Pause(SHOW_CARDS_PAUSE_MS), 
                    Action.Render(End(winners, newMatched)))
                  else
                    Seq(Action.Render(Play(players, newBoard, GoodMatch, newMatched, List())), 
                    Action.Pause(SHOW_CARDS_PAUSE_MS), 
                    Action.Render(Play(players, board.updated(selected(0), entry0matched).updated(selected(1), entry1matched), SelectingCards, newMatched, List())))
                else
                  val entry0notMatched = (board(selected(0))._1, FaceDown)
                  val entry1notMatched = (board(selected(1))._1, FaceDown)
                  Seq(Action.Render(Play(players, board.updated(selected(0), entry0).updated(selected(1), entry1), BadMatch, alreadyMatched, List())), 
                  Action.Pause(SHOW_CARDS_PAUSE_MS), 
                  Action.Render(Play(players.tail.:+(userId), board.updated(selected(0), entry0notMatched).updated(selected(1), entry1notMatched), SelectingCards, alreadyMatched, List())))
          else
            throw NotYourTurnException()
        case End(winners, alreadyMatched) => throw IllegalMoveException("already over!")
        case _ => throw IllegalMoveException("undefined state")

  override def project(state: MemoryState)(userId: UserId): MemoryView =
    state match
      case Play(players, board, SelectingCards, alreadyMatched, selected) => 
        if userId == players.head then
          MemoryView(Playing(SelectingCards, players.head, board.map(_._2).toSeq), alreadyMatched)
        else
          MemoryView(Playing(Waiting, players.head, board.map(_._2).toSeq), alreadyMatched)
      case Play(players, board, CardsSelected, alreadyMatched, selected) => 
        if userId == players.head then
          MemoryView(Playing(CardsSelected, players.head, board.map(_._2).toSeq), alreadyMatched)
        else
          MemoryView(Playing(Waiting, players.head, board.map(_._2).toSeq), alreadyMatched)
      case Play(players, board, phase, alreadyMatched, selected) => 
        MemoryView(Playing(phase, players.head, board.map(_._2).toSeq), alreadyMatched)
      case End(winners, alreadyMatched) => MemoryView(Finished(winners), alreadyMatched)

// Server registration magic
class register:
  WebServer.register(MemoryStateMachine)
