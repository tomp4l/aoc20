package aoc20
package day7

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.implicits._

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(7)
      .flatMap { lines =>
        val bags = Bags.fromStrings(lines)
        for {
          canContainGoldBags <- bags.canContainGoldBags
          _ <- Console.output(1, canContainGoldBags.length)
          countBagsFromGold <- bags.countBagsFromGold
          _ <- Console.output(2, countBagsFromGold)
        } yield ()
      }
      .as(ExitCode.Success)

}

case class Bag(name: String, otherBags: Vector[(String, Int)])

object Bag {
  def fromString(string: String): Bag = {
    val split = string.split(" ")
    val (nameParts, rest) = split.splitAt(2)

    val otherBags = rest
      .sliding(4)
      .map(_.toSeq match {
        case Seq("contain", "no", "other", "bags.") => None
        case Seq(amount, adj, colour, bag) if bag.take(3) == "bag" =>
          Some((s"$adj $colour", amount.toInt))
        case _ => None
      })
      .flatten
      .toVector

    Bag(nameParts.mkString(" "), otherBags)
  }
}

case class Bags(map: Map[String, Bag]) {
  def canContainGoldBags: IO[Vector[Bag]] =
    map.values.toVector
      .filterA(bag => containsBagIO(bag, "shiny gold"))

  private def containsBagIO(bag: Bag, name: String): IO[Boolean] =
    if (bag.otherBags.exists { case (n, _) => n == name }) true.pure[IO]
    else
      bag.otherBags
        .flatMap { case (n, _) =>
          map
            .get(n)
            .map(b => containsBagIO(b, name))
        }
        .existsM(identity)

  def countBagsFromGold: IO[Int] = {
    val gold = map("shiny gold")
    def countRecursive(bag: Bag): IO[Int] =
      bag.otherBags
        .map { case (name, amount) =>
          countRecursive(map(name)).map(_ * amount)
        }
        .sequence
        .map(_.sum + 1)
    countRecursive(gold).map(_ - 1)
  }
}

object Bags {

  def fromStrings(strings: Vector[String]): Bags =
    Bags(strings.map(Bag.fromString).map(bag => bag.name -> bag).toMap)
}
