package aoc20
package day11

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import scala.annotation.tailrec

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(11)
      .flatMap { lines =>
        val initial = Space.mapFromStrings(lines)
        for {
          _ <- Console.output(
            1,
            Occupancy
              .stabilise(initial, Occupancy.directlyAdjacent)
              .values
              .count(_ == Occupied),
          )
          _ <- Console.output(
            2,
            Occupancy
              .stabilise(initial, Occupancy.visiablyAdjacent, 5)
              .values
              .count(_ == Occupied),
          )
        } yield ()
      }
      .as(ExitCode.Success)

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

  def mapFromStrings(s: Seq[String]): Map[(Int, Int), Space] =
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

  val directlyAdjacent: ((Int, Int), Map[(Int, Int), Space]) => Seq[Space] =
    (coord, spaces) => {
      val adjacent = for {
        x <- -1 to 1
        y <- -1 to 1
        if x != 0 || y != 0
      } yield (coord._1 + x, coord._2 + y)
      adjacent.flatMap(spaces.get)
    }

  val visiablyAdjacent: ((Int, Int), Map[(Int, Int), Space]) => Seq[Space] =
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
    spaces: Map[(Int, Int), Space],
    adjacentSpaces: ((Int, Int), Map[(Int, Int), Space]) => Seq[Space],
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
    spaces: Map[(Int, Int), Space],
    adjacentSpace: ((Int, Int), Map[(Int, Int), Space]) => Seq[Space],
    tolerance: Int,
  ): Map[(Int, Int), Space] =
    spaces.map { case (coord, _) =>
      coord -> newOccupancyForSpace(coord, spaces, adjacentSpace, tolerance)
    }

  @tailrec
  def stabilise(
    spaces: Map[(Int, Int), Space],
    adjacentSpace: ((Int, Int), Map[(Int, Int), Space]) => Seq[Space],
    tolerance: Int = 4,
  ): Map[(Int, Int), Space] = {
    val next = newSpaces(spaces, adjacentSpace, tolerance)
    if (next == spaces) next
    else stabilise(next, adjacentSpace, tolerance)
  }

  def doPrint(spaces: Map[(Int, Int), Space]) =
    spaces.toSeq
      .map { case ((a, b), c) => ((b, a), c) }
      .sortBy(_._1)
      .foreach { case ((_, y), c) =>
        if (y == 0) {
          print('\n')
        }
        print(c.asChar)
      }
}
