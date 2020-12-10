package aoc20
package day01

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
    val s = numbers.toSet
    val pairs = for {
      x <- numbers
      y = 2020 - x
      if s.contains(y)
    } yield (x * y)
    pairs.headOption
  }

  private def find2020Triple(numbers: Vector[Long]): Option[Long] = {
    val s = numbers.toSet
    val pairs = for {
      x <- numbers
      y <- numbers
      z = 2020 - x - y
      if s.contains(z)
    } yield (x * y * z)
    pairs.headOption
  }
}
