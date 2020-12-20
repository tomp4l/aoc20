package aoc20
package day19

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(19)
      .flatMap { lines =>
        val processableRule =
          RecursiveProcessableRule.fromRules(
            lines.flatMap(Rule.fromString),
            false,
          )
        val replacedRule =
          RecursiveProcessableRule.fromRules(
            lines.flatMap(Rule.fromString),
            true,
          )
        val words = lines.filter(_.headOption.exists(c => c == 'a' || c == 'b'))
        for {
          _ <- Console.output(1, countMatches(processableRule, words))
          _ <- Console.output(2, countMatches(replacedRule, words))
        } yield ()
      }
      .as(ExitCode.Success)

  private def countMatches(
    rule: Option[RecursiveProcessableRule],
    words: Vector[String],
  ) =
    words
      .flatMap(w => rule.map(RecursiveProcessableRule.matches(_, w)))
      .count(identity)
}

sealed trait RuleLogic

object RuleLogic {
  case class Or(l: RuleLogic, r: RuleLogic) extends RuleLogic
  case class CharRule(c: Char) extends RuleLogic
  case class OtherRule(id: Int) extends RuleLogic
  case class OtherRules(id1: Int, id2: Int) extends RuleLogic

  val OrPattern = """(.*)\|(.*)""".r
  val CharPattern = """"(.)"""".r
  val OtherRulePattern = """(\d+)""".r
  val OtherRulesPattern = """(\d+) (\d+)""".r

  def fromString(s: String): Option[RuleLogic] = s.trim match {
    case OrPattern(l, r) =>
      for {
        ll <- fromString(l)
        rl <- fromString(r)
      } yield Or(ll, rl)
    case CharPattern(c) => Some(CharRule(c.head))
    case OtherRulePattern(i) => Some(OtherRule(i.toInt))
    case OtherRulesPattern(i, j) => Some(OtherRules(i.toInt, j.toInt))
    case s =>
      println(s)
      None
  }
}

case class Rule(id: Int, logic: RuleLogic)

object Rule {
  def fromString(string: String): Option[Rule] =
    for {
      i <- string.indexOfOption(':')
      (idS, rest) = string.splitAt(i)
      id <- idS.toIntOption
      logic <- RuleLogic.fromString(rest.tail)
    } yield Rule(id, logic)
}

sealed trait Match {
  def isSuccess: Boolean
}
case class SuccessfulMatch(rem: String) extends Match {
  val isSuccess = true
}
case object FailedMatch extends Match {
  val isSuccess = false
}

trait RecursiveProcessableRule {
  def matches(s: String): Seq[String]
}

object RecursiveProcessableRule {

  def matches(r: RecursiveProcessableRule, s: String): Boolean =
    r.matches(s).exists(_ == "")

  case class Or(l: RecursiveProcessableRule, r: RecursiveProcessableRule)
      extends RecursiveProcessableRule {
    override def matches(s: String): Seq[String] =
      l.matches(s) ++ r.matches(s)
  }
  case class CharRule(c: Char) extends RecursiveProcessableRule {
    override def matches(s: String): Seq[String] =
      if (s.headOption == Some(c)) Seq(s.tail)
      else Seq()
  }

  case class OtherRules(
    l: RecursiveProcessableRule,
    r: RecursiveProcessableRule,
  ) extends RecursiveProcessableRule {
    override def matches(s: String): Seq[String] =
      l.matches(s).flatMap { rem =>
        r.matches(rem)
      }
  }

  case class RepeatedRule(r: RecursiveProcessableRule)
      extends RecursiveProcessableRule {
    override def matches(s: String): Seq[String] =
      r.matches(s) match {
        case Seq() => Seq()
        case s => s ++ s.flatMap(matches(_))
      }
  }

  case class SandwichRule(
    l: RecursiveProcessableRule,
    r: RecursiveProcessableRule,
  ) extends RecursiveProcessableRule {
    override def matches(s: String): Seq[String] =
      l.matches(s) match {
        case Seq() => Seq()
        case s =>
          s.flatMap(r.matches(_)) ++
            s.flatMap(matches(_)).flatMap(r.matches(_))
      }
  }

  def fromRules(seq: Seq[Rule], replace: Boolean) = {
    def map = seq.map(r => r.id -> r.logic).toMap
    map.get(0).flatMap(fromRuleLogic(_, map, replace: Boolean))
  }

  private def fromId(
    id: Int,
    others: Map[Int, RuleLogic],
    replace: Boolean,
  ): Option[RecursiveProcessableRule] = (replace, id) match {
    case (true, 8) =>
      others
        .get(42)
        .flatMap(l => (fromRuleLogic(l, others, replace)))
        .map(RepeatedRule(_))
    case (true, 11) =>
      for {
        l <- others.get(42)
        r <- others.get(31)
        ll <- fromRuleLogic(l, others, replace)
        rl <- fromRuleLogic(r, others, replace)
      } yield SandwichRule(ll, rl)
    case (_, id) =>
      others
        .get(id)
        .flatMap(fromRuleLogic(_, others, replace))
  }

  private def fromRuleLogic(
    rule: RuleLogic,
    others: Map[Int, RuleLogic],
    replace: Boolean,
  ): Option[RecursiveProcessableRule] = rule match {
    case RuleLogic.Or(l, r) =>
      for {
        ll <- fromRuleLogic(l, others, replace)
        rl <- fromRuleLogic(r, others, replace)
      } yield (Or(ll, rl))
    case RuleLogic.CharRule(c) => Some(CharRule(c))
    case RuleLogic.OtherRule(id) =>
      fromId(id, others, replace)
    case RuleLogic.OtherRules(id1, id2) =>
      for {
        ll <- fromId(id1, others, replace)
        rl <- fromId(id2, others, replace)
      } yield OtherRules(ll, rl)
  }
}
