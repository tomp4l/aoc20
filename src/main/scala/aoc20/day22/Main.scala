package aoc20
package day22

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.implicits.*

object Main extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(22)
      .flatMap { lines =>
        val player1 = Player.fromStrings(lines.takeWhile(_ != ""))
        val player2 = Player.fromStrings(lines.dropWhile(_ != "").tail)
        val game = (player1, player2).mapN(Combat(_, _))
        val rGame = (player1, player2).mapN(RecusiveCombat(_, _))
        for
          _ <- Console.outputOptional(1, game.map(Game.score(_)))
          _ <- Console.outputOptional(1, rGame.map(Game.score(_)))
        yield ()
      }
      .as(ExitCode.Success)

case class Player(number: Int, deck: Seq[Int]):
  def score =
    if deck.isEmpty then 0
    else deck.reverse.zip(1 to deck.length).map { case (a, b) => a * b }.sum

object Player:
  def fromStrings(strings: Seq[String]): Option[Player] =
    val player = strings.head
    val number = player.dropRight(1).lastOption.flatMap(_.toString.toIntOption)
    val deck = strings.tail.map(_.toIntOption).sequence
    for
      number <- number
      deck <- deck
    yield Player(number, deck)

trait Game[A]:
  def isWon(a: A): Boolean
  def next(a: A): A
  def score(a: A): Int

object Game:
  def runToWin[A](g: A)(using game: Game[A]): A =
    if game.isWon(g) then g
    else runToWin(game.next(g))

  def score[A](g: A)(using game: Game[A]): Int = game.score(runToWin(g))
case class Combat(player1: Player, player2: Player):
  def isWon = player1.deck.isEmpty || player2.deck.isEmpty

  def next: Combat =
    (player1.deck.headOption, player2.deck.headOption)
      .mapN { case (c1, c2) =>
        val (d1, d2) =
          if c1 > c2 then (player1.deck.tail :+ c1 :+ c2, player2.deck.tail)
          else (player1.deck.tail, player2.deck.tail :+ c2 :+ c1)
        Combat(player1.copy(deck = d1), player2.copy(deck = d2))
      }
      .getOrElse(this)

  def score = player1.score + player2.score

object Combat:
  given Game[Combat] with
    def isWon(a: Combat): Boolean = a.isWon
    def next(a: Combat): Combat = a.next
    def score(a: Combat) = a.score

case class RecusiveCombat(
  player1: Player,
  player2: Player,
  previousGames: Set[(Player, Player)] = Set.empty,
):
  private def seenPrevious = previousGames.contains((player1, player2))

  def isWon =
    seenPrevious || player1.deck.isEmpty || player2.deck.isEmpty

  def winner =
    if seenPrevious then player1.number
    else if player1.deck.isEmpty then player2.number
    else player1.number

  def next =
    (player1.deck.headOption, player2.deck.headOption)
      .mapN { case (c1, c2) =>
        val d1 = player1.deck.tail
        val d2 = player2.deck.tail
        def outcome(player1Wins: Boolean) =
          if player1Wins then (d1 :+ c1 :+ c2, d2)
          else (d1, d2 :+ c2 :+ c1)
        val (d1n, d2n) = if d1.length >= c1 && d2.length >= c2 then
          val p1 = player1.copy(deck = d1.take(c1))
          val p2 = player2.copy(deck = d2.take(c2))
          val recursiveGame = RecusiveCombat(p1, p2)
          val won = Game.runToWin(recursiveGame)
          outcome(won.winner == player1.number)
        else outcome(c1 > c2)
        RecusiveCombat(
          player1.copy(deck = d1n),
          player2.copy(deck = d2n),
          previousGames + ((player1, player2)),
        )
      }
      .getOrElse(this)

  def score =
    if winner == player1.number then player1.score else player2.score

object RecusiveCombat:
  given Game[RecusiveCombat] = new Game[RecusiveCombat] {
    def isWon(a: RecusiveCombat): Boolean = a.isWon
    def next(a: RecusiveCombat): RecusiveCombat = a.next
    def score(a: RecusiveCombat) = a.score
  }
