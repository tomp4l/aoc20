package aoc20.day18

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExpressionSpec extends AnyFlatSpec with Matchers:

  it should "parse 1 + 2" in {
    Expression.parseLine("1 + 2").run should be(
      BigInt(3),
    )
  }

  it should "parse 1 + 2 * 3 + 4 * 5 + 6" in {
    Expression.parseLine("1 + 2 * 3 + 4 * 5 + 6").run should be(
      BigInt(71),
    )
  }

  it should "parse 2 * 3 + (4 * 5)" in {
    Expression.parseLine("2 * 3 + (4 * 5)").run should be(
      BigInt(26),
    )
  }

  it should "parse 5 + (8 * 3 + 9 + 3 * 4 * 3)" in {
    Expression
      .parseLine(
        "5 + (8 * 3 + 9 + 3 * 4 * 3)",
      )
      .run should be(
      BigInt(437),
    )
  }

  it should "parse 5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" in {
    Expression
      .parseLine("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
      .run should be(
      BigInt(12240),
    )
  }

  it should "parse ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" in {
    Expression
      .parseLine("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
      .run should be(
      BigInt(13632),
    )
  }
