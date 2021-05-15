package aoc20
package day24

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.data.Chain
import aoc20.cellularautomaton.Automaton
import aoc20.cellularautomaton.Coord

object Main extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(24)
      .flatMap { lines =>
        val directions = lines.flatMap(Direction.fromString)
        for
          _ <- Console.output(1, Worker.flipAndCountBlack(directions))
          _ <- Console.output(2, Worker.run100Days(directions))
        yield ()
      }
      .as(ExitCode.Success)

sealed trait Direction
case object East extends Direction
case object West extends Direction
case object SouthEast extends Direction
case object SouthWest extends Direction
case object NorthEast extends Direction
case object NorthWest extends Direction

object Direction:
  def fromString(s: String): Option[Seq[Direction]] =
    def loop(
      remaining: String,
      directions: Chain[Direction] = Chain.empty,
    ): Option[Chain[Direction]] =
      remaining.headOption match
        case Some('e') => loop(remaining.tail, directions :+ East)
        case Some('w') => loop(remaining.tail, directions :+ West)
        case None => Some(directions)
        case _ =>
          remaining.take(2) match
            case "se" => loop(remaining.drop(2), directions :+ SouthEast)
            case "sw" => loop(remaining.drop(2), directions :+ SouthWest)
            case "ne" => loop(remaining.drop(2), directions :+ NorthEast)
            case "nw" => loop(remaining.drop(2), directions :+ NorthWest)
            case _ => None
    loop(s).map(_.toList)

case class HexaganolCoordinate(x: Int, y: Int):
  def move(d: Direction) = d match
    case East => copy(x = x + 1)
    case West => copy(x = x - 1)
    case SouthEast => copy(y = y - 1)
    case SouthWest => copy(x = x - 1, y = y - 1)
    case NorthEast => copy(x = x + 1, y = y + 1)
    case NorthWest => copy(y = y + 1)

  def neighbours: Seq[HexaganolCoordinate] = Seq(
    move(East),
    move(West),
    move(SouthEast),
    move(SouthWest),
    move(NorthEast),
    move(NorthWest),
  )

object HexaganolCoordinate:
  final val Origin = HexaganolCoordinate(0, 0)
  given Coord[HexaganolCoordinate] = a => a.neighbours

case class Floor(tiles: Set[HexaganolCoordinate]):
  def flip(coord: HexaganolCoordinate): Floor =
    if tiles.contains(coord) then Floor(tiles - coord)
    else Floor(tiles + coord)

object Floor:
  def empty = Floor(Set.empty)

object Worker:
  def initialState(directions: Seq[Seq[Direction]]): Floor =
    directions.foldLeft(Floor.empty) { (f, directions) =>
      val finalCoord = directions.foldLeft((HexaganolCoordinate.Origin)) {
        (currentCoord, direction) =>
          currentCoord.move(direction)
      }
      f.flip(finalCoord)
    }

  def flipAndCountBlack(directions: Seq[Seq[Direction]]): Int =
    initialState(directions).tiles.size

  def run100Days(directions: Seq[Seq[Direction]]) =
    val initial = initialState(directions)
    Automaton
      .runN(
        initial.tiles,
        i => i == 1 || i == 2,
        i => i == 2,
        100,
      )
      .size
