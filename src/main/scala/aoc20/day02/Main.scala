package aoc20
package day02

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import scala.util.Try

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(2)
      .flatMap(lines =>
        for {
          _ <- Console
            .output(1, countPassword(lines, Validators.MinMaxValidator))
          _ <- Console
            .output(2, countPassword(lines, Validators.PositionValidator))

        } yield (),
      )
      .as(ExitCode.Success)

  private def countPassword(lines: Seq[String], validator: Validator) =
    lines
      .map(PasswordWithPolicy.parse(validator))
      .filter(_.map(_.isValid).getOrElse(false))
      .size

}

trait Validator {
  def isValid(policy: PasswordPolicy, password: String): Boolean
}

object Validators {
  val MinMaxValidator: Validator = (policy: PasswordPolicy, password: String) =>
    {
      import policy._
      val charCount = password.filter(_ == char).size
      charCount >= a && charCount <= b
    }

  val PositionValidator: Validator =
    (policy: PasswordPolicy, password: String) => {
      import policy._
      val c1 = password.lift(a - 1)
      val c2 = password.lift(b - 1)
      (c1, c2) match {
        case (Some(c1), Some(c2)) if (c1 == char || c2 == char) && c1 != c2 =>
          true
        case _ => false
      }
    }
}

case class PasswordPolicy(
  a: Int,
  b: Int,
  char: Char,
  validator: Validator,
) {
  def isValid(password: String) = validator.isValid(this, password)
}

case class PasswordWithPolicy(password: String, policy: PasswordPolicy) {
  def isValid = policy.isValid(password)
}

object PasswordWithPolicy {
  final val Syntax = """(\d+)-(\d+) (.): (.*)""".r

  def parse(
    validator: Validator,
  )(line: String): Option[PasswordWithPolicy] = line match {
    case Syntax(a, b, char, password) =>
      Try(
        PasswordWithPolicy(
          password,
          PasswordPolicy(
            a.toInt,
            b.toInt,
            char.head,
            validator,
          ),
        ),
      ).toOption
    case _ => None
  }
}
