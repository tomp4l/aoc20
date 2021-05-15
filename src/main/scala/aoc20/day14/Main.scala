package aoc20
package day14

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(14)
      .flatMap { lines =>
        val instructions = Instruction.fromStrings(lines)
        val computer = Computer.run(instructions)
        val computer2 = ComputerV2.run(instructions)

        for
          _ <- Console.outputOptional(1, computer.map(_.state.values.sum))
          _ <- Console.outputOptional(2, computer2.map(_.state.values.sum))
        yield ()
      }
      .as(ExitCode.Success)

sealed trait Instruction
case class SetMask(mask: String) extends Instruction
case class SetMemory(address: Long, value: Long) extends Instruction

object Instruction:
  final private val MaskPattern = """mask = (.*)""".r
  final private val MemoryPattern = """mem\[(\d+)\] = (.*)""".r

  def fromStrings(s: Seq[String]): Seq[Instruction] =
    s.map {
      case MaskPattern(mask) => SetMask(mask)
      case MemoryPattern(address, value) =>
        SetMemory(address.toLong, value.toLong)
    }

case class Computer(mask: BitMask, state: Map[Long, Long] = Map.empty):
  def applyInstruction(instruction: Instruction): Computer = instruction match
    case SetMask(mask) => copy(mask = BitMask(mask))
    case SetMemory(address, value) =>
      copy(state = state + (address -> mask(value)))

object Computer:
  def run(instructions: Seq[Instruction]): Option[Computer] =
    instructions.headOption
      .flatMap {
        case SetMask(mask) => Some(BitMask(mask))
        case SetMemory(_, _) => None
      }
      .map { mask =>
        val initial = Computer(mask)
        instructions.tail.foldLeft(initial)(_.applyInstruction(_))
      }

case class BitMask(mask: String):
  private val overwriteBits = mask.map {
    case '1' => Some(true)
    case '0' => Some(false)
    case _ => None
  }.reverse

  def apply(l: Long): Long = overwriteBits
    .foldLeft((l, 1L)) { case ((acc, bit), mask) =>
      mask match
        case Some(true) => (acc | bit, bit << 1)
        case Some(false) => (acc & (~bit), bit << 1)
        case None => (acc, bit << 1)
    }
    ._1

case class BitMaskV2(mask: String):
  private val overwriteBits = mask.map {
    case '1' => Some(true)
    case '0' => Some(false)
    case _ => None
  }.reverse

  def apply(l: Long): Seq[Long] = overwriteBits
    .foldLeft((Vector(l), 1L)) { case ((acc, bit), mask) =>
      mask match
        case Some(true) => (acc.map(l => l | bit), bit << 1)
        case Some(false) => (acc, bit << 1)
        case None =>
          val notBit = ~bit
          val next = acc.flatMap(l => Vector(l | bit, l & notBit))
          (next, bit << 1)
    }
    ._1

case class ComputerV2(mask: BitMaskV2, state: Map[Long, Long] = Map.empty):
  def applyInstruction(instruction: Instruction): ComputerV2 =
    instruction match
      case SetMask(mask) => copy(mask = BitMaskV2(mask))
      case SetMemory(address, value) =>
        val newState = mask(address).foldLeft(state)((s, a) => s + (a -> value))
        copy(state = newState)

object ComputerV2:
  def run(instructions: Seq[Instruction]): Option[ComputerV2] =
    instructions.headOption
      .flatMap {
        case SetMask(mask) => Some(BitMaskV2(mask))
        case SetMemory(_, _) => None
      }
      .map { mask =>
        val initial = ComputerV2(mask)
        instructions.tail.foldLeft(initial)(_.applyInstruction(_))
      }
