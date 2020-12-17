package aoc20
package day17

import scala.util.chaining._
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(17)
      .flatMap { lines =>
        val cubes3d = Cubes.fromStrings[Coord3d](lines)
        val cubes4d = Cubes.fromStrings[Coord4d](lines)
        for {
          _ <- Console.output(1, run6(cubes3d))
          _ <- Console.output(2, run6(cubes4d))
        } yield ()
      }
      .as(ExitCode.Success)

  private def run6(cubes: Cubes[_]) =
    (1 to 6).foldLeft(cubes)((c, _) => c.next).powered.size
}

trait Coord[A] {
  def neighbours(a: A): Seq[A]
  def plane(x: Int, y: Int): A
}

case class Coord3d(x: Int, y: Int, z: Int) {
  def neighbours = for {
    dx <- -1 to 1
    dy <- -1 to 1
    dz <- -1 to 1
    if dx != 0 || dy != 0 || dz != 0
  } yield Coord3d(x + dx, y + dy, z + dz)
}

object Coord3d {
  implicit val coordInstance: Coord[Coord3d] = new Coord[Coord3d] {
    override def neighbours(a: Coord3d): Seq[Coord3d] = a.neighbours
    override def plane(x: Int, y: Int): Coord3d = Coord3d(x, y, 0)
  }
}

case class Coord4d(w: Int, x: Int, y: Int, z: Int) {
  def neighbours = for {
    dw <- -1 to 1
    dx <- -1 to 1
    dy <- -1 to 1
    dz <- -1 to 1
    if dw != 0 || dx != 0 || dy != 0 || dz != 0
  } yield Coord4d(w + dw, x + dx, y + dy, z + dz)
}

object Coord4d {
  implicit val coordInstance: Coord[Coord4d] = new Coord[Coord4d] {
    override def neighbours(a: Coord4d): Seq[Coord4d] = a.neighbours
    override def plane(x: Int, y: Int): Coord4d = Coord4d(x, y, 0, 0)
  }
}

case class Cubes[A](powered: Set[A])(implicit coord: Coord[A]) {
  def next: Cubes[A] = {
    val changeCandidates = powered ++ powered.flatMap(coord.neighbours(_))
    changeCandidates
      .foldLeft(Set.empty[A]) { (s, c) =>
        val isPowered = powered(c)
        val poweredNeighbours = coord.neighbours(c).count(powered(_))
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
  def fromStrings[A](
    strings: Seq[String],
  )(implicit coord: Coord[A]): Cubes[A] = {
    val powered = strings.zipWithIndex.foldLeft(Set.empty[A]) {
      case (s, (line, y)) =>
        line.zipWithIndex.foldLeft(s) {
          case (s, ('#', x)) => s + coord.plane(x, y)
          case (s, _) => s
        }
    }
    Cubes(powered)
  }
}
