package aoc20
package day18

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

import scala.util.matching.Regex

object Main extends IOApp:

  override def run(args: List[String]): IO[ExitCode] = readInputLines(18)
    .flatMap { lines =>
      for
        _ <- Console.output(1, partOne(lines))
        _ <- Console.output(2, partTwo(lines))
      yield ()
    }
    .as(ExitCode.Success)

  def partOne(lines: Vector[String]) =
    lines.map(Expression.parseLine).map(_.run).sum

  def partTwo(lines: Vector[String]) =
    lines.map(Expression2.parseLine).map(_.run).sum

sealed trait Expression:
  def run: Long

case class IntExpression(x: Int) extends Expression:
  def run: Long = x.toLong

case class AddExpression(a: Expression, b: Expression) extends Expression:
  override def run: Long = a.run + b.run

case class Multiplication(a: Expression, b: Expression) extends Expression:
  override def run: Long = a.run * b.run

trait Parsers[Parser[_]]:
  self =>
  def run[A](p: Parser[A])(input: String): Option[A]

  def string(s: String): Parser[String]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def regex(r: Regex): Parser[String]

  def succeed[A](a: A): Parser[A]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => map(p2)(b => (a, b)))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for
      a <- p
      b <- p2
    yield f(a, b)

  extension [A](p1: Parser[A])
    def |(p2: Parser[A]): Parser[A] = self.or(p1, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B]

    def map[B](f: A => B): Parser[B] = p1.flatMap(v => succeed(f(v)))

    def ~[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p1, p2)

    def many: Parser[List[A]] =
      map2(p1, self.many[A](p1))(_ :: _) | succeed(List())

object Parsers:
  opaque type Parser[A] = (s: String) => Option[(String, A)]

  given parsers: Parsers[Parser] with
    def run[A](p: Parser[A])(input: String): Option[A] =
      p(input).filter(_._1.isEmpty).map(_._2)

    def string(s: String): Parser[String] =
      v =>
        if v.startsWith(s) then Some((v.drop(s.length), s))
        else None

    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
      s =>
        p1(s) match
          case None => p2(s)
          case v => v

    extension [A](p: Parser[A])
      def flatMap[B](f: A => Parser[B]): Parser[B] =
        s =>
          p(s) match
            case Some((r, a)) => f(a)(r)
            case None =>
              None

    def regex(r: Regex): Parser[String] = s =>
      r.findPrefixOf(s) match
        case None => None
        case Some(m) => Some((s.drop(m.length), m))

    def succeed[A](a: A): Parser[A] = s => Some((s, a))

object Expression:

  import Parsers.*
  import Parsers.parsers.*

  def number: Parser[Expression] =
    regex(" *[123456789]+ *".r)
      .map(_.trim.toInt)
      .map(IntExpression(_))

  def parens: Parser[Expression] =
    (regex(" *\\( *".r) ~ addMul ~ regex(" *\\) *".r)).map(_._1._2)

  def factor: Parser[Expression] = number | parens

  def addMul: Parser[Expression] =
    (factor ~ (regex("\\+|\\*".r) ~ factor).many).map { case (l, r) =>
      r.foldLeft(l) { case (o, (op, b)) =>
        op match
          case "*" => Multiplication(o, b)
          case "+" => AddExpression(o, b)
      }
    }

  def expression = addMul

  def parseLine(line: String) =
    Parsers.parsers.run(expression)(line).get

object Expression2:
  import Parsers.*
  import Parsers.parsers.*

  def number: Parser[Expression] =
    regex(" *[123456789]+ *".r)
      .map(_.trim.toInt)
      .map(IntExpression(_))

  def parens: Parser[Expression] =
    (regex(" *\\( *".r) ~ mul ~ regex(" *\\) *".r)).map(_._1._2)

  def factor: Parser[Expression] = number | parens

  def add = (factor ~ (string("+") ~ factor).many).map { case (l, r) =>
    r.foldLeft(l) { case (o, (_, b)) =>
      AddExpression(o, b)
    }
  }

  def mul = (add ~ (string("*") ~ add).many).map { case (l, r) =>
    r.foldLeft(l) { case (o, (_, b)) =>
      Multiplication(o, b)
    }
  }

  def expression = mul

  def parseLine(line: String) =
    Parsers.parsers.run(expression)(line).get
