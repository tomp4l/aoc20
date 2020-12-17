package aoc20
package day17

import scala.util.chaining._
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(17)
      .flatMap { lines =>
        val cubes3d = Cubes.fromStrings3d(lines)
        val cubes4d = Cubes.fromStrings4d(lines)
        for {
          _ <- Console.output(1, run6(cubes3d))
          _ <- Console.output(2, run6(cubes4d))
        } yield ()
      }
      .as(ExitCode.Success)

  private def run6(cubes: Cubes) =
    (1 to 6).foldLeft(cubes)((c, _) => c.next).powered.size
}

trait Coord {
  def neighbours: Seq[Coord]
}

case class Coord3d(x: Int, y: Int, z: Int) extends Coord {
  def neighbours = for {
    dx <- -1 to 1
    dy <- -1 to 1
    dz <- -1 to 1
    if dx != 0 || dy != 0 || dz != 0
  } yield Coord3d(x + dx, y + dy, z + dz)
}

case class Coord4d(w: Int, x: Int, y: Int, z: Int) extends Coord {
  def neighbours = for {
    dw <- -1 to 1
    dx <- -1 to 1
    dy <- -1 to 1
    dz <- -1 to 1
    if dw != 0 || dx != 0 || dy != 0 || dz != 0
  } yield Coord4d(w + dw, x + dx, y + dy, z + dz)
}

case class Cubes(powered: Set[Coord]) {
  def next: Cubes = {
    val changeCandidates = powered ++ powered.flatMap(_.neighbours)
    changeCandidates
      .foldLeft(Set.empty[Coord]) { (s, c) =>
        val isPowered = powered(c)
        val poweredNeighbours = c.neighbours.count(powered(_))
        if (
          isPowered && Set(2, 3).contains(
            poweredNeighbours,
          ) || !isPowered && poweredNeighbours == 3
        ) s + c
        else s
      }
      .pipe(Cubes(_))
  }
}

object Cubes {

  private def fromString(
    strings: Seq[String],
    factory: (Int, Int) => Coord,
  ): Cubes = {
    val powered = strings.zipWithIndex.foldLeft(Set.empty[Coord]) {
      case (s, (line, y)) =>
        line.zipWithIndex.foldLeft(s) {
          case (s, ('#', x)) => s + factory(x, y)
          case (s, _) => s
        }
    }
    Cubes(powered)
  }

  def fromStrings3d(strings: Seq[String]): Cubes =
    fromString(strings, (x, y) => Coord3d(x, y, 0))

  def fromStrings4d(strings: Seq[String]): Cubes =
    fromString(strings, (x, y) => Coord4d(x, y, 0, 0))
}
