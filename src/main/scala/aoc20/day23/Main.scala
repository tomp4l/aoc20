package aoc20
package day23

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    IO.blocking {
      val cups = Cups.fromString("784235916")
      (1 to 100).foldLeft(cups)((c, _) => c.next)
    }.flatMap(oneHundred =>
      Console.output(1, oneHundred.fromOne.tail.mkString),
    ) *>
      IO.blocking {
        val cups = Cups.fromStringMillion("784235916")
        (1 to 10_000_000).foldLeft(cups)((c, _) => c.next)
      }.flatMap(tenMillion =>
        Console
          .output(2, tenMillion.fromOne.tail.take(2).map(_.toLong).product),
      ).as(ExitCode.Success)

case class Cups private (
  cups: Map[Int, Int],
  position: Int,
  max: Int,
):
  private def locate(n: Int, not: Seq[Int]): Int =
    val m = n - 1
    val mPos = if m < 1 then max else m
    if not.contains(mPos) then locate(mPos, not)
    else mPos

  def next: Cups =
    val a = cups(position)
    val b = cups(a)
    val c = cups(b)
    val three = List(a, b, c)
    val fourth = cups(c)
    val next = locate(position, three)
    val updates = List(position -> fourth, next -> a, c -> cups(next))
    val nextCups = cups ++ updates
    new Cups(cups = nextCups, nextCups(position), max)

  def fromOne =
    def loop(
      v: Int,
      acc: List[Int] = List.empty,
    ): List[Int] =
      val p = cups(v)
      if p == 1 then acc.reverse
      else loop(p, p :: acc)
    1 :: loop(1)

object Cups:
  def fromString(string: String) =
    val cups = string.toVector.map(_.toString.toInt)
    Cups(cups)

  def fromStringMillion(string: String) =
    val cups = string.toVector.map(_.toString.toInt)
    Cups((cups ++ (10 to 1_000_000)))

  def apply(l: Seq[Int]): Cups =
    val map =
      l.sliding(2).foldLeft(Map(l.last -> l.head)) { (map, neighbours) =>
        val a = neighbours.head
        val b = neighbours.last
        map + (a -> b)
      }
    Cups(map, l.head, l.max)
