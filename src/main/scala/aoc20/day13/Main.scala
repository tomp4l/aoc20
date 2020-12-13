package aoc20
package day13

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.implicits._
import scala.util.chaining._

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(13)
      .flatMap { lines =>
        val time = lines.head.toInt
        val busses = lines.last.split(',').toVector
        for {

          _ <- Console.output(1, firstBus(time, busses))
          _ <- Console.output(2, earliestTime(busses))

        } yield ()

      }
      .as(ExitCode.Success)

  def firstBus(time: Int, busIds: Vector[String]) = {
    val numericIds = busIds.flatMap(_.toIntOption)

    numericIds
      .map { id =>
        id -> timeToBus(id, time)
      }
      .minBy(_._2)
      .pipe { case (a, b) => a * b }
  }

  def earliestTime(busIds: Vector[String]): Long = {
    val bussesWithRemainders = busIds.zipWithIndex
      .flatMap { case (id, index) =>
        id.toIntOption.map(i => i -> ((i * (1 + index / i) - index) % i))
      }
      .map { case (i, j) => i.toLong -> j.toLong }

    def seive(
      targets: Vector[(Long, Long)],
      currentCheck: Long,
      currentTarget: Long,
      currentMod: Long,
      step: Long,
    ): Long =
      if (currentCheck % currentMod == currentTarget) {
        if (targets.isEmpty) currentCheck
        else {
          val (id, rem) = targets.head
          seive(targets.tail, currentCheck, rem, id, step * currentMod)
        }
      } else
        seive(targets, currentCheck + step, currentTarget, currentMod, step)

    val (id, rem) = bussesWithRemainders.head

    seive(bussesWithRemainders.tail, rem, rem, id, 1)
  }

  private def timeToBus(id: Int, time: Int) = {
    val rem = time % id
    if (rem > 0)
      id - rem
    else 0
  }
}
