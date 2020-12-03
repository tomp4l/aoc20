package aoc20
package day1

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLongs(1)
      .flatMap(n =>
        for {
          _ <- output(1, find2020Pair(n))
          _ <- output(2, find2020Triple(n))
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

  private def output(part: Int, value: Option[Long]) = value match {
    case Some(v) => Console.writeLine(s"Part $part: $v")
    case None => Console.error(s"Failed to complete part $part")
  }

}
