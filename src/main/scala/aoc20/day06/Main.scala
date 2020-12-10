package aoc20
package day06

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInput(6)
      .map(_.split("\n\n").toVector)
      .flatMap(groups =>
        for {
          _ <- Console.output(1, countAnyYes(groups))
          _ <- Console.output(2, countAllYes(groups))
        } yield (),
      )
      .as(ExitCode.Success)

  def countAnyYes(groups: Vector[String]): Int =
    groups.map { g =>
      g.split("\n").flatten.toSet.size
    }.sum

  def countAllYes(groups: Vector[String]): Int =
    groups.map { g =>
      g.split("\n").map(_.toSet).reduce(_.intersect(_)).size
    }.sum
}
