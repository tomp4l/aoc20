package aoc20
package day04

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    readInput(4)
      .map(_.split("\n\n").toVector)
      .flatMap { lines =>
        val passports = Passports.fromStrings(lines)
        for
          _ <- Console.output(1, passports.size)
          _ <- Console.output(2, passports.count(_.isValid))
        yield ()
      }
      .as(ExitCode.Success)

case class Passport(
  birthYear: String,
  issueYear: String,
  expirationYear: String,
  height: String,
  hairColour: String,
  eyeColour: String,
  passportId: String,
  countryId: Option[String],
):
  def isValid =
    validNumber(birthYear, 1920, 2002) &&
      validNumber(issueYear, 2010, 2020) &&
      validNumber(expirationYear, 2020, 2030) &&
      validHeight(height) &&
      validHairColour(hairColour) &&
      validEyeColour(eyeColour) &&
      validPassortId(passportId)

  private def validNumber(number: String, min: Int, max: Int) =
    number.toIntOption
      .filter(_ >= min)
      .filter(_ <= max)
      .isDefined

  private def validHeight(height: String) =
    val (n, unit) = height.splitAt(height.size - 2)
    unit match
      case "in" => validNumber(n, 59, 76)
      case "cm" => validNumber(n, 150, 193)
      case _ => false

  private def validHairColour(colour: String) =
    Passport.HairColourPattern.matches(colour)

  private def validEyeColour(colour: String) =
    List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(colour)

  private def validPassortId(passportId: String) =
    Passport.PassportIdPattern.matches(passportId)

object Passport:
  final private val HairColourPattern = """^#[0-9a-f]{6}$""".r
  final private val PassportIdPattern = """^[0-9]{9}$""".r

  def fromMap(map: Map[String, String]) = for
    birthYear <- map.get("byr")
    issueYear <- map.get("iyr")
    expirationYear <- map.get("eyr")
    height <- map.get("hgt")
    hairColour <- map.get("hcl")
    eyeColour <- map.get("ecl")
    passportId <- map.get("pid")
  yield Passport(
    birthYear,
    issueYear,
    expirationYear,
    height,
    hairColour,
    eyeColour,
    passportId,
    map.get("cid"),
  )

object Passports:
  def fromStrings(strings: Seq[String]): Seq[Passport] =
    strings.map { line =>
      val lineAsMap = line
        .split(" |\n")
        .map(_.split(":").toList)
        .collect { case List(k, v) =>
          (k, v)
        }
        .toMap
      Passport.fromMap(lineAsMap)
    }.flatten
