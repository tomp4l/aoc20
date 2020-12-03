package aoc20
package day1

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLongs(1)
      .flatMap(n =>
        for {
          _ <- Console.outputOptional(1, find2020Pair(n))
          _ <- Console.outputOptional(2, find2020Triple(n))
        } yield (),
      )
      .as(ExitCode.Success)

  private def find2020Pair(numbers: Vector[Long]): Option[Long] = {
    val pairs = for {
      x <- numbers
      y <- numbers
      if x + y == 2020
    } yield (x * y)
    pairs.headOption
  }

  private def find2020Triple(numbers: Vector[Long]): Option[Long] = {
    val pairs = for {
      x <- numbers
      y <- numbers
      z <- numbers
      if x + y + z == 2020
    } yield (x * y * z)
    pairs.headOption
  }
}
