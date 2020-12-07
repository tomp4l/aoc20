package aoc20.day7

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AsyncFlatSpec

import cats.effect.unsafe.implicits.global

class BagSpec extends AsyncFlatSpec with Matchers {

  it should "parse a bag" in {
    val bag = Bag.fromString(
      "light tomato bags contain 1 plaid fuchsia bag, 5 dark gray bags, 2 striped tan bags, 5 striped lavender bags.",
    )

    bag.unsafeToFuture().map { bag =>
      bag.name should be("light tomato")
      bag.otherBags.size should be(4)
      bag.otherBags.find(_._1 == "dark gray") should be(Some(("dark gray", 5)))
    }
  }

  it should "match directly containing bag" in {
    val bags =
      Bags.fromStrings(Vector("bright white bags contain 1 shiny gold bag."))

    bags.flatMap(_.canContainGoldBags).unsafeToFuture().map(_.size should be(1))
  }

  it should "match deeply containing bag" in {
    val bags =
      Bags.fromStrings(
        Vector(
          "bright white bags contain 1 shiny gold bag.",
          "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
        ),
      )

    bags.flatMap(_.canContainGoldBags).unsafeToFuture().map(_.size should be(2))
  }

  it should "match all bags containing gold" in {
    val bags =
      Bags.fromStrings(
        """light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.""".split("\n").toVector,
      )

    bags.flatMap(_.canContainGoldBags).unsafeToFuture().map(_.size should be(4))
  }

  it should "count all bags in gold" in {
    val bags =
      Bags.fromStrings(
        """light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.""".split("\n").toVector,
      )

    bags.flatMap(_.countBagsFromGold).unsafeToFuture().map(_ should be(32))
  }
}
