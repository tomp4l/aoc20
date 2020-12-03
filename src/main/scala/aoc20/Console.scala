package aoc20

import cats.effect.IO

object Console {
  def writeLine(x: String) = IO(println(x))

  def error(e: String) = IO(
    println(s"${scala.Console.RED}$e${scala.Console.RESET}"),
  )

  def output(part: Int, value: Any) = writeLine(s"Part $part: $value")

  def outputOptional(part: Int, value: Option[Any]) = value match {
    case Some(v) => output(part, v)
    case None => error(s"Failed to complete part $part")
  }
}
