package aoc20
package day17

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import aoc20.cellularautomaton.Automaton
import aoc20.cellularautomaton.Coord

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(17)
      .flatMap { lines =>
        val cubes3d = Cubes.fromStrings[Coord3d](lines)
        val cubes4d = Cubes.fromStrings[Coord4d](lines)
        for {
          _ <- Console.output(1, Cubes.run6(cubes3d).size)
          _ <- Console.output(2, Cubes.run6(cubes4d).size)
        } yield ()
      }
      .as(ExitCode.Success)
}

trait OnPlane[A] {
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
  implicit val planeInstance: OnPlane[Coord3d] = (x, y) => Coord3d(x, y, 0)
  implicit val coordInstance: Coord[Coord3d] = c => c.neighbours
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
  implicit val planeInstance: OnPlane[Coord4d] = (x, y) => Coord4d(x, y, 0, 0)
  implicit val coordInstance: Coord[Coord4d] = c => c.neighbours
}

object Cubes {
  def fromStrings[A: Coord](
    strings: Seq[String],
  )(implicit onPlane: OnPlane[A]): Set[A] = {
    val powered = strings.zipWithIndex.foldLeft(Set.empty[A]) {
      case (s, (line, y)) =>
        line.zipWithIndex.foldLeft(s) {
          case (s, ('#', x)) => s + onPlane.plane(x, y)
          case (s, _) => s
        }
    }
    powered
  }

  def run6[A: Coord](powered: Set[A]) =
    Automaton.runN(powered, i => i == 2 || i == 3, i => i == 3, 6)
}
