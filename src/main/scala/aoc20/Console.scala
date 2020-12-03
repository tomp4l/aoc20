package aoc20

import cats.effect.IO

object Console {
  def writeLine(x: String) = IO(println(x))

  def error(e: String) = IO(
    println(s"${scala.Console.RED}$e${scala.Console.RESET}"),
  )
}
