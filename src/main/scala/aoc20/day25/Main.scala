package aoc20
package day25

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp {

  val cardPublic = 2069194
  val doorPublic = 16426071
  final val Modulus = 20201227
  final val InitialSubject = 7

  override def run(args: List[String]): IO[ExitCode] =
    (IO {
      val c = findLoop(cardPublic)
      val d = findLoop(doorPublic)
      val k1 = loop(c, doorPublic)
      val k2 = loop(d, cardPublic)
      assert(k1 == k2)
      k1
    } flatMap (v => Console.output(1, v))).as(ExitCode.Success)

  private def findLoop(
    target: Int,
    currentLoop: Int = 1,
    previous: Int = 1,
  ): Int = {
    val next = (previous * InitialSubject) % Modulus
    if (next == target) {
      currentLoop
    } else findLoop(target, currentLoop + 1, next)
  }

  private def loop(loops: Int, subject: Int) =
    BigInt(subject).modPow(BigInt(loops), BigInt(Modulus)).toInt
}
