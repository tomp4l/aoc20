package aoc20
package day09

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLongs(9)
      .flatMap { numbers =>
        val invalidNumber = findFirstInvalid(numbers)
        for {
          _ <- Console.outputOptional(1, invalidNumber)
          _ <- Console.outputOptional(
            2,
            invalidNumber.flatMap(findContiguous(numbers)),
          )
        } yield ()
      }
      .as(ExitCode.Success)

  def findFirstInvalid(numbers: Vector[Long]): Option[Long] =
    numbers
      .sliding(26)
      .find { slice =>
        val search = slice.init
        val target = slice.last
        val s = search.toSet
        !search.map(v => target - v).exists(s.contains(_))
      }
      .map(_.last)

  def findContiguous(numbers: Vector[Long])(target: Long): Option[Long] =
    numbers.tails
      .flatMap(
        _.inits.toVector.reverse
          .takeWhile(_.sum <= target)
          .lastOption
          .find(_.sum == target),
      )
      .find(_.nonEmpty)
      .map(v => v.min + v.max)
}
