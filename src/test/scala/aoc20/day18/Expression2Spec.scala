package aoc20.day18

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Expression2Spec extends AnyFlatSpec with Matchers:

  it should "parse 1 + 2" in {
    Expression2.parseLine("1 + 2").run should be(
      BigInt(3),
    )
  }

  it should "parse 1 + 2 * 3 + 4 * 5 + 6" in {
    Expression2.parseLine("1 + 2 * 3 + 4 * 5 + 6").run should be(
      BigInt(231),
    )
  }

  it should "parse 2 * 3 + (4 * 5)" in {
    Expression2.parseLine("2 * 3 + (4 * 5)").run should be(
      BigInt(46),
    )
  }

  it should "parse 5 + (8 * 3 + 9 + 3 * 4 * 3)" in {
    Expression2
      .parseLine(
        "5 + (8 * 3 + 9 + 3 * 4 * 3)",
      )
      .run should be(
      BigInt(1445),
    )
  }

  it should "parse 5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" in {
    Expression2
      .parseLine("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
      .run should be(
      BigInt(669060),
    )
  }

  it should "parse ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" in {
    Expression2
      .parseLine("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
      .run should be(
      BigInt(23340),
    )
  }
