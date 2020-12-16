package aoc20
package day16

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(16)
      .flatMap { lines =>
        val tickets = Tickets.fromStrings(lines)
        for {
          _ <- Console.output(1, tickets.scanningErrorRate)
          _ <- Console.output(2, getSumDeparture(tickets))
        } yield ()
      }
      .as(ExitCode.Success)

  def getSumDeparture(tickets: Tickets) = {
    val orderedRules = tickets.orderedRules
    orderedRules.zipWithIndex
      .filter(_._1.name.contains("departure"))
      .map(i => tickets.ourTicket.fields(i._2).toLong)
      .product
  }
}

sealed trait Rule {
  def matches(number: Int): Boolean
}

case class Range(min: Int, max: Int) extends Rule {
  override def matches(number: Int): Boolean = number >= min && number <= max
}

case class Or(left: Rule, right: Rule) extends Rule {
  override def matches(number: Int): Boolean =
    left.matches(number) || right.matches(number)
}

case class NamedRule(name: String, rule: Rule)

object NamedRule {
  val Pattern = """(.*): (\d+)-(\d+) or (\d+)-(\d+)""".r

  def fromString(string: String) =
    string match {
      case Pattern(name, a, b, c, d) =>
        Some(
          NamedRule(name, Or(Range(a.toInt, b.toInt), Range(c.toInt, d.toInt))),
        )
      case _ => None
    }
}

case class Ticket(fields: IndexedSeq[Int]) {
  def errorRate(rules: Seq[Rule]): Int =
    fields.filterNot(f => rules.exists(_.matches(f))).sum
}

object Ticket {
  def fromString(string: String) =
    Ticket(string.split(',').flatMap(_.toIntOption).toVector)
}

case class Tickets(
  rules: Seq[NamedRule],
  ourTicket: Ticket,
  otherTickets: Seq[Ticket],
) {
  private val unnamedRuled = rules.map(_.rule)

  def scanningErrorRate =
    otherTickets.map(_.errorRate(unnamedRuled)).sum

  val validTickets = otherTickets.filter(_.errorRate(unnamedRuled) == 0)

  def orderedRules = {

    def loop(
      index: Int,
      remainingRules: Seq[NamedRule],
      accum: List[NamedRule],
    ): Option[Seq[NamedRule]] = if (remainingRules.isEmpty) Some(accum)
    else {
      val possibilities = validTickets.flatMap(_.fields.lift(index))
      val matchingRules =
        remainingRules.filter(r => possibilities.forall(r.rule.matches(_)))
      matchingRules.foldLeft[Option[Seq[NamedRule]]](None)((a, r) =>
        if (a.isDefined) a
        else loop(index + 1, remainingRules.filterNot(_ == r), r :: accum),
      )
    }

    loop(0, rules, List()).map(_.reverse).getOrElse(List())
  }
}

object Tickets {
  def fromStrings(strings: Seq[String]) = {
    val (rules, rest) =
      strings.partitionMap(s => NamedRule.fromString(s).toLeft(s))
    val ourTicketRest = rest.dropWhile(_ != "your ticket:").drop(1)
    val ourTicket = ourTicketRest.headOption.map(Ticket.fromString).get
    val otherTicketsRest =
      ourTicketRest.dropWhile(_ != "nearby tickets:").drop(1)
    val otherTickets = otherTicketsRest.map(Ticket.fromString)
    Tickets(rules, ourTicket, otherTickets)
  }
}
