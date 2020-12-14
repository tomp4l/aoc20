package aoc20.day14

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BitMaskSpec extends AnyFlatSpec with Matchers {

  it should "mask a number with bits to change" in {
    val b = BitMask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
    b(11) should be(73)
  }

  it should "mask a number with no bits to change" in {
    val b = BitMask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
    b(101) should be(101)
  }

  it should "mask zero with bits to change" in {
    val b = BitMask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
    b(0) should be(64)
  }

  it should "handle a big mask" in {
    val b = BitMask("10111X1110001X10110X0010011X01011100")

    b(4) should be(
      java.lang.Long.parseLong("101110111000101011000010011001011100", 2),
    )
  }

  "V2" should "handle all possibilities" in {
    val b = BitMaskV2("000000000000000000000000000000X1001X")

    b(42).length should be(4)
    b(42).toVector.sorted should be(Vector(26, 27, 58, 59))
  }
}
