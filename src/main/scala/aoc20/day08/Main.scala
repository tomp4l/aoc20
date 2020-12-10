package aoc20
package day08

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.implicits._
import scala.annotation.tailrec

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(8)
      .flatMap { lines =>
        val console = GameConsole.fromStrings(lines)
        for {
          _ <- Console.output(
            1,
            runToLoopOrTermination(console).swap
              .map(_.toString)
              .getOrElse("Terminated?"),
          )
          _ <- Console.output(2, findTerminating(console))
        } yield ()
      }
      .as(ExitCode.Success)

  def runToLoopOrTermination(console: GameConsole): Either[Int, Int] = {
    @tailrec
    def loop(console: GameConsole, visited: Set[Int]): Either[Int, Int] =
      console.next match {
        case Some(next) =>
          val pointer = next.pointer
          if (visited.contains(pointer)) console.acc.asLeft
          else loop(next, visited + pointer)
        case None => console.acc.asRight
      }
    loop(console, Set(console.pointer))
  }

  def findTerminating(console: GameConsole): Int = {
    @tailrec
    def loop(pointer: Int): Int = {
      val ins = console.instructions(pointer)
      ins.op match {
        case Acc => loop(pointer + 1)
        case Jmp | Nop =>
          val newInstructions =
            console.instructions.updated(
              pointer,
              ins.copy(op = if (ins.op == Jmp) Nop else Jmp),
            )
          runToLoopOrTermination(
            console.copy(instructions = newInstructions),
          ) match {
            case Left(_) => loop(pointer + 1)
            case Right(value) => value
          }
      }
    }
    loop(0)
  }
}

trait Operation
case object Acc extends Operation
case object Jmp extends Operation
case object Nop extends Operation

case class Instruction(op: Operation, arg: Int)

case class GameConsole(
  instructions: Vector[Instruction],
  acc: Int = 0,
  pointer: Int = 0,
) {
  def next: Option[GameConsole] =
    instructions.get(pointer.toLong).map { instruction =>
      instruction.op match {
        case Acc => copy(acc = acc + instruction.arg, pointer = pointer + 1)
        case Jmp => copy(pointer = pointer + instruction.arg)
        case Nop => copy(pointer = pointer + 1)
      }
    }
}

object GameConsole {
  def fromStrings(strings: Vector[String]): GameConsole = GameConsole(
    strings.map(Instruction.fromString),
  )
}

object Instruction {
  def fromString(string: String): Instruction = {
    val (op, arg) = string.split(" ").toList.splitAt(1).tupled.head
    Instruction(Operation.fromString(op), arg.toInt)
  }
}

object Operation {
  def fromString(string: String): Operation = string match {
    case "nop" => Nop
    case "acc" => Acc
    case "jmp" => Jmp
  }
}
