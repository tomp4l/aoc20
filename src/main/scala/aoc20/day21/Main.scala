package aoc20
package day21

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(21)
      .flatMap { lines =>
        val ingredients = lines.flatMap(Ingredients.fromString)
        for {
          _ <- Console.output(1, findNonAllergents(ingredients))
          _ <- Console.output(1, identifyAllergens(ingredients))
        } yield ()
      }
      .as(ExitCode.Success)

  private def potentialAllergens(ingredients: Seq[Ingredients]) = {
    val allAllergens = ingredients.flatMap(_.allergens)
    allAllergens.map(allergen =>
      allergen ->
        ingredients
          .filter(_.allergens.contains(allergen))
          .map(_.ingredients.toSet)
          .reduce(_ intersect _),
    )
  }

  private def findNonAllergents(ingredients: Seq[Ingredients]) = {
    val translated = potentialAllergens(ingredients).flatMap(_._2).toSet
    ingredients.flatMap(_.ingredients.filterNot(translated.contains(_))).length
  }

  private def identifyAllergens(ingredients: Seq[Ingredients]) = {
    val potentialTranslations = potentialAllergens(ingredients)

    def translate(
      potentialTranslations: Seq[(String, Set[String])],
      translated: Map[String, String] = Map.empty,
    ): Map[String, String] =
      if (potentialTranslations.isEmpty) translated
      else {
        val (allergen, translations) =
          potentialTranslations.find(_._2.size == 1).get
        val translation = translations.head
        val nextTranslations =
          potentialTranslations.filterNot(_._1 == allergen).map { case (a, t) =>
            a -> (t - translation)
          }
        translate(nextTranslations, translated + (allergen -> translation))
      }
    translate(potentialTranslations).toSeq.sortBy(_._1).map(_._2).mkString(",")
  }
}

case class Ingredients(ingredients: Seq[String], allergens: Seq[String])

object Ingredients {
  final private val Pattern = """^(.*) \(contains (.*)\)""".r
  def fromString(s: String): Option[Ingredients] = s match {
    case Pattern(ingredientString, allergenString) =>
      Some(
        Ingredients(
          ingredientString.split(" ").toVector,
          allergenString.split(", ").toVector,
        ),
      )
    case _ => None
  }
}
