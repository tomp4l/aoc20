package aoc20
package day05

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(5)
      .flatMap { lines =>
        val ids = lines.map(BoardingPass.calculateId)
        for {
          _ <- Console.output(1, ids.max)
          _ <- Console.outputOptional(2, findSeat(ids))

        } yield ()
      }
      .as(ExitCode.Success)

  def findSeat(ids: Vector[Int]) = {
    val min = ids.min
    val max = ids.max
    val s = ids.toSet
    (min to max).find(i => !s.contains(i))
  }
}

object BoardingPass {

  def calculateId(bsp: String): Int = {
    val (fb, lr) = bsp.splitAt(7)
    calculateRow(fb) * 8 + calculateColumn(lr)
  }

  def calculateRow(fb: String): Int = Bsp.bsp(
    fb.map {
      case 'F' => Bsp.Lower
      case 'B' => Bsp.Upper
    },
    0,
    127,
  )

  def calculateColumn(lr: String): Int = Bsp.bsp(
    lr.map {
      case 'L' => Bsp.Lower
      case 'R' => Bsp.Upper
    },
    0,
    7,
  )

}

object Bsp {
  sealed trait Partition
  case object Upper extends Partition
  case object Lower extends Partition

  def bsp(partitions: Seq[Partition], min: Int, max: Int): Int =
    partitions
      .foldLeft((min, max)) { case ((min, max), p) =>
        val midpoint = (min + max) / 2
        p match {
          case Upper => (midpoint + 1, max)
          case Lower => (min, midpoint)
        }
      }
      ._1
}
