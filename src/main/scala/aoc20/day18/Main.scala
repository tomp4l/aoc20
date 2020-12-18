package aoc20
package day18

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

import fastparse._
import SingleLineWhitespace._

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = readInputLines(18)
    .flatMap { lines =>
      for {
        _ <- Console.output(1, partOne(lines))
        _ <- Console.output(2, partTwo(lines))
      } yield ()
    }
    .as(ExitCode.Success)

  def partOne(lines: Vector[String]) =
    lines.map(Expression.parseLine).map(_.run).sum

  def partTwo(lines: Vector[String]) =
    lines.map(Expression2.parseLine).map(_.run).sum
}

sealed trait Expression {
  def run: Long
}

case class IntExpression(x: Int) extends Expression {
  def run: Long = x.toLong
}
case class AddExpression(a: Expression, b: Expression) extends Expression {
  override def run: Long = a.run + b.run
}
case class Multiplication(a: Expression, b: Expression) extends Expression {
  override def run: Long = a.run * b.run
}

object Expression {

  def number[_: P]: P[Expression] =
    CharsWhileIn("123456789").!.map(_.toInt).map(IntExpression(_))

  def parens[_: P]: P[Expression] = P("(" ~/ addMul ~ ")")

  def factor[_: P]: P[Expression] = P(number | parens)

  def addMul[_: P]: P[Expression] = P(
    factor ~ (CharIn("+*").! ~/ factor).rep,
  ).map { case (l, r) =>
    r.foldLeft(l) { case (o, (op, b)) =>
      op match {
        case "*" => Multiplication(o, b)
        case "+" => AddExpression(o, b)
      }
    }
  }

  def expression[_: P]: P[Expression] = P(addMul ~ End)

  def parseLine(line: String) =
    parse(line, expression(_)).get.value
}

object Expression2 {

  def number[_: P]: P[Expression] =
    CharsWhileIn("123456789").!.map(_.toInt).map(IntExpression(_))

  def parens[_: P]: P[Expression] = P("(" ~/ mul ~ ")")

  def factor[_: P]: P[Expression] = P(number | parens)

  def add[_: P]: P[Expression] = P(
    factor ~ ("+" ~/ factor).rep,
  ).map { case (l, r) =>
    r.foldLeft(l) { case (o, (b)) =>
      AddExpression(o, b)
    }
  }

  def mul[_: P]: P[Expression] = P(
    add ~ ("*" ~/ add).rep,
  ).map { case (l, r) =>
    r.foldLeft(l) { case (o, (b)) =>
      Multiplication(o, b)
    }
  }

  def expression[_: P]: P[Expression] = P(mul ~ End)

  def parseLine(line: String) =
    parse(line, expression(_)).get.value
}
