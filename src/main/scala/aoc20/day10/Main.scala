package aoc20
package day10

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLongs(10)
      .flatMap { numbers =>
        for {
          _ <- Console.output(1, find1or3Jolts(numbers))
          _ <- Console.output(2, countValidCombinations(numbers))
        } yield ()
      }
      .as(ExitCode.Success)

  def find1or3Jolts(adapters: Vector[Long]): Int = {
    val withBaseAndBuiltin =
      (Vector(0, adapters.max + 3) ++ adapters).sorted
    val differences =
      withBaseAndBuiltin.sliding(2).map(_.reduce((a, b) => b - a)).toVector
    val oneJolt = differences.count(_ == 1)
    val threeJolt = differences.count(_ == 3)
    oneJolt * threeJolt
  }

  def countValidCombinations(adapters: Vector[Long]): Long = {
    val withBaseAndBuiltin =
      (Vector(0, adapters.max + 3) ++ adapters).sorted
    val differences =
      withBaseAndBuiltin.sliding(2).map(_.reduce((a, b) => b - a)).toVector
    differences.mkString
      .split("3")
      .map(_.length)
      .map({
        case 0 => 1L
        case 1 => 1L
        case 2 => 2L
        case 3 => 4L
        case 4 => 7L
      })
      .product
  }
}
