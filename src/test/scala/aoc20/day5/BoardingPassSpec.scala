package aoc20.day05

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoardingPassSpec extends AnyFlatSpec with Matchers {

  it should "find the correct row for a binary space partitioning" in {
    BoardingPass.calculateRow("FBFBBFF") should be(44)
  }

  it should "find the correct column for a binary space partitioning" in {
    BoardingPass.calculateColumn("RLR") should be(5)
  }

  it should "find the correct ID for a binary space partitioning" in {
    BoardingPass.calculateId("FBFBBFFRLR") should be(357)
  }

  it should "find the correct ID for other binary space partitioning" in {
    BoardingPass.calculateId("BFFFBBFRRR") should be(567)
    BoardingPass.calculateId("FFFBBBFRRR") should be(119)
    BoardingPass.calculateId("BBFFBBFRLL") should be(820)
  }
}
