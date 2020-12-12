package aoc20
package day11

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import scala.annotation.tailrec
import cats.implicits._

object Main extends IOApp {

  import Occupancy._

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(11)
      .flatMap { lines =>
        val initial = Space.mapFromStrings(lines)
        val both = (
          stabilise(initial, 4, directlyAdjacent),
          stabilise(initial, 5, visiablyAdjacent),
        ).parTupled
        for {
          b <- both
          _ <- Console.output(1, b._1)
          _ <- Console.output(2, b._2)
        } yield ()
      }
      .as(ExitCode.Success)

  private def stabilise(
    initial: SpaceMap,
    tolerance: Int,
    adjacentSpaces: AdjacentSpaces,
  ) = IO {
    Occupancy
      .stabilise(initial, adjacentSpaces, tolerance)
      .values
      .count(_ == Occupied)
  }
}

trait Space {
  def asChar: Char
}

object Space {
  def fromChar(c: Char): Space = c match {
    case '.' => Floor
    case 'L' => Empty
    case '#' => Occupied
  }

  def mapFromStrings(s: Seq[String]): Occupancy.SpaceMap =
    s.zipWithIndex.flatMap { case (s, y) =>
      s.map(fromChar).zipWithIndex.map { case (s, x) => (x, y) -> s }
    }.toMap
}

case object Floor extends Space {

  override def asChar: Char = '.'

}
case object Empty extends Space {

  override def asChar: Char = 'L'

}
case object Occupied extends Space {

  override def asChar: Char = '#'

}

object Occupancy {

  trait AdjacentSpaces extends Function2[(Int, Int), SpaceMap, Seq[Space]]
  type SpaceMap = Map[(Int, Int), Space]

  val directlyAdjacent: AdjacentSpaces =
    (coord, spaces) => {
      val adjacent = for {
        x <- -1 to 1
        y <- -1 to 1
        if x != 0 || y != 0
      } yield (coord._1 + x, coord._2 + y)
      adjacent.flatMap(spaces.get)
    }

  val visiablyAdjacent: AdjacentSpaces =
    (coord, spaces) => {
      val adjacent = for {
        x <- -1 to 1
        y <- -1 to 1
        if x != 0 || y != 0
      } yield (x, y)
      val adjacentCoords = adjacent
        .map { case (x, y) =>
          LazyList
            .iterate(coord) { case (x2, y2) =>
              (x + x2, y + y2)
            }
            .tail
        }
        .flatMap(_.find(c => spaces.get(c).map(_ != Floor).getOrElse(true)))
      adjacentCoords.flatMap(spaces.get)
    }

  def newOccupancyForSpace(
    coord: (Int, Int),
    spaces: SpaceMap,
    adjacentSpaces: AdjacentSpaces,
    tolerance: Int,
  ): Space = {
    val adjacentSpace = adjacentSpaces(coord, spaces)
    spaces(coord) match {
      case Floor => Floor
      case Empty =>
        if (adjacentSpace.forall(_ != Occupied)) Occupied
        else Empty
      case Occupied =>
        if (adjacentSpace.count(_ == Occupied) >= tolerance) Empty
        else Occupied
    }
  }

  def newSpaces(
    spaces: SpaceMap,
    adjacentSpace: AdjacentSpaces,
    tolerance: Int,
  ): SpaceMap =
    spaces.map { case (coord, _) =>
      coord -> newOccupancyForSpace(coord, spaces, adjacentSpace, tolerance)
    }

  @tailrec
  def stabilise(
    spaces: SpaceMap,
    adjacentSpace: AdjacentSpaces,
    tolerance: Int = 4,
  ): SpaceMap = {
    val next = newSpaces(spaces, adjacentSpace, tolerance)
    if (next == spaces) next
    else stabilise(next, adjacentSpace, tolerance)
  }

  def doPrint(spaces: SpaceMap): IO[Unit] = IO {
    spaces.toSeq
      .map { case ((a, b), c) => ((b, a), c) }
      .sortBy(_._1)
      .foreach { case ((_, y), c) =>
        if (y == 0) {
          print('\n')
        }
        print(c.asChar)
      }
    print("\n")
  }
}
