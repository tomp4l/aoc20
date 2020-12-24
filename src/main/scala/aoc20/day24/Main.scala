package aoc20
package day24

import fastparse._, NoWhitespace._
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import aoc20.cellularautomaton.Automaton
import aoc20.cellularautomaton.BooleanIsomorphism
import aoc20.cellularautomaton.Coord

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(24)
      .flatMap { lines =>
        val directions = lines.flatMap(Direction.fromString)
        for {
          _ <- Console.output(1, Worker.flipAndCountBlack(directions))
          _ <- Console.output(2, Worker.run100Days(directions))
        } yield ()
      }
      .as(ExitCode.Success)
}

sealed trait Direction
case object East extends Direction
case object West extends Direction
case object SouthEast extends Direction
case object SouthWest extends Direction
case object NorthEast extends Direction
case object NorthWest extends Direction

object Direction {
  def parser[_: P]: P[Seq[Direction]] = P(
    (("se" | "sw" | "ne" | "nw" | "e" | "w").!.rep ~ End).map(s =>
      s.map({
        case "se" => SouthEast
        case "sw" => SouthWest
        case "ne" => NorthEast
        case "nw" => NorthWest
        case "e" => East
        case "w" => West
      }),
    ),
  )

  def fromString(s: String) =
    parse(s, parser(_)).fold((_, _, _) => None, (v, _) => Some(v))
}

sealed trait TileColour {
  def flip: TileColour
}
case object Black extends TileColour {
  def flip: TileColour = White
}
case object White extends TileColour {
  def flip: TileColour = Black
}

object TileColour {
  implicit val booleanInstance: BooleanIsomorphism[TileColour] =
    new BooleanIsomorphism[TileColour] {
      def truthy: TileColour = Black
      def fromBool(b: Boolean): TileColour = if (b) Black else White
      def value(a: TileColour): Boolean = a match {
        case Black => true
        case White => false
      }
    }
}

case class HexaganolCoordinate(x: Int, y: Int) {
  def move(d: Direction) = d match {
    case East => copy(x = x + 1)
    case West => copy(x = x - 1)
    case SouthEast => copy(y = y - 1)
    case SouthWest => copy(x = x - 1, y = y - 1)
    case NorthEast => copy(x = x + 1, y = y + 1)
    case NorthWest => copy(y = y + 1)
  }

  def neighbours: Seq[HexaganolCoordinate] = Seq(
    move(East),
    move(West),
    move(SouthEast),
    move(SouthWest),
    move(NorthEast),
    move(NorthWest),
  )
}

object HexaganolCoordinate {
  final val Origin = HexaganolCoordinate(0, 0)
  implicit val coord: Coord[HexaganolCoordinate] =
    a => a.neighbours
}

case class Floor(tiles: Map[HexaganolCoordinate, TileColour]) {
  def flip(coord: HexaganolCoordinate): Floor = {
    val colour = tiles.getOrElse(coord, White)
    Floor(tiles + (coord -> colour.flip))
  }
}

object Floor {
  def empty = Floor(Map.empty)
}

object Worker {
  def initialState(directions: Seq[Seq[Direction]]): Floor =
    directions.foldLeft(Floor.empty) { (f, directions) =>
      val finalCoord = directions.foldLeft((HexaganolCoordinate.Origin)) {
        (currentCoord, direction) =>
          currentCoord.move(direction)
      }
      f.flip(finalCoord)
    }

  def flipAndCountBlack(directions: Seq[Seq[Direction]]): Int =
    initialState(directions).tiles.values.count(_ == Black)

  def run100Days(directions: Seq[Seq[Direction]]) = {
    val initial = initialState(directions)
    Automaton
      .runN[HexaganolCoordinate, Map[?, TileColour]](
        initial.tiles,
        i => i == 1 || i == 2,
        i => i == 2,
        100,
      )
      .values
      .count(_ == Black)
  }
}
