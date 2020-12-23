package aoc20
package day23

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    IO {
      val cups = Cups.fromString("784235916")
      (1 to 100).foreach(_ => cups.next())
      cups
    }.flatMap(oneHundred =>
      Console.output(1, oneHundred.fromOne.tail.mkString),
    ) *>
      IO {
        val cups = Cups.fromStringMillion("784235916")
        (1 to 10_000_000).foreach(_ => cups.next())
        cups
      }.flatMap(tenMillion =>
        Console.output(2, tenMillion.fromOne.tail.take(2).map(_.toLong).product),
      ).as(ExitCode.Success)

}

class Cups private (
  var cups: MutableLinkedList[Int],
  lookup: Map[Int, MutableLinkedList[Int]],
  max: Int,
) {

  private def locate(n: Int, not: Seq[Int]): MutableLinkedList[Int] = {
    val m = n - 1
    val mPos = if (m < 1) max else m
    if (not.contains(mPos)) locate(mPos, not)
    else lookup(mPos)
  }

  def next(): Unit = {
    val a = cups.next
    val b = a.next
    val c = b.next
    val three = List(a, b, c).map(_.value)

    val fourth = c.next
    cups.next = fourth
    fourth.previous = cups

    val next = locate(cups.value, three)
    val nextNext = next.next
    next.next = a
    a.previous = next
    nextNext.previous = c
    c.next = nextNext
    cups = cups.next
  }

  def fromOne = {
    def loop(
      list: MutableLinkedList[Int],
      acc: List[Int] = List.empty,
    ): List[Int] = {
      val p = list.previous
      if (p.value == 1) {
        1 :: acc
      } else {
        loop(p, p.value :: acc)
      }
    }
    loop(lookup(1))
  }
}

object Cups {
  def fromString(string: String) = {
    val cups = string.toVector.map(_.toString.toInt)
    Cups(MutableLinkedList.fromSeq(cups))
  }

  def fromStringMillion(string: String) = {
    val cups = string.toVector.map(_.toString.toInt)
    Cups(MutableLinkedList.fromSeq(cups ++ (10 to 1_000_000)))
  }

  def apply(l: MutableLinkedList[Int]): Cups = {
    def loop(
      head: MutableLinkedList[Int],
      acc: Map[Int, MutableLinkedList[Int]],
    ): Map[Int, MutableLinkedList[Int]] =
      if (head == l) {
        acc + (head.value -> head)
      } else {
        val n = acc + (head.value -> head)
        loop(head.next, n)
      }
    val lookup = loop(l.next, Map.empty)
    val max = lookup.keys.max
    new Cups(l, lookup, max)
  }
}

final class MutableLinkedList[A](val value: A) {
  var previous: MutableLinkedList[A] = _
  var next: MutableLinkedList[A] = _
}

object MutableLinkedList {
  def fromSeq(s: Seq[Int]) = {
    val mapped = s.map(new MutableLinkedList(_))
    mapped.sliding(2).foreach { neighbours =>
      val a = neighbours.head
      val b = neighbours.last
      a.next = b
      b.previous = a
    }

    val head = mapped.head
    val last = mapped.last
    head.previous = last
    last.next = head

    mapped.head
  }
}
