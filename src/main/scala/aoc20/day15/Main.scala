package aoc20
package day15

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    IO("10,16,6,0,1,17")
      .flatMap { line =>
        val numbers = line.split(",").map(_.toInt).toVector
        for
          _ <- Console.output(1, game(numbers, 2020))
          _ <- Console.output(2, game(numbers, 30000000))
        yield ()
      }
      .as(ExitCode.Success)

  def game(numbers: Vector[Int], targetTurn: Int): Int =
    val occurances: Map[Int, List[Int]] = numbers.zipWithIndex.map {
      case (a, b) => a -> List(1 + b)
    }.toMap

    def loop(occurances: Map[Int, List[Int]], turn: Int, previous: Int): Int =
      val p = occurances(previous)
      val n = p match
        case a :: b :: _ => a - b
        case _ => 0
      if turn >= targetTurn then n
      else
        loop(
          occurances + (n -> (turn :: occurances.getOrElse(n, List()).take(2))),
          turn + 1,
          n,
        )

    loop(occurances, occurances.size + 1, numbers.last)
