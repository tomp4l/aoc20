package aoc20
package day3

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(3)
      .flatMap { lines =>
        val parsed = lines.map(RepeatedBiome.fromString)
        for {
          _ <- Console.output(1, xSlope(parsed, 3))
          _ <- Console.output(2, partTwo(parsed))
        } yield ()
      }
      .as(ExitCode.Success)

  def partTwo(biomes: Seq[RepeatedBiome]) = {
    val a = xSlope(biomes, 1)
    val b = xSlope(biomes, 3)
    val c = xSlope(biomes, 5)
    val d = xSlope(biomes, 7)
    val e = ySlope(biomes, 2)
    a * b * c * d * e
  }

  def xSlope(biomes: Seq[RepeatedBiome], slope: Int): Long =
    biomes
      .foldLeft((0, 0)) { case ((index, count), repeatedBiome) =>
        val biome = repeatedBiome.at(index)
        (index + slope, count + (if (biome == Tree) 1 else 0))
      }
      ._2
      .toLong

  def ySlope(biomes: Seq[RepeatedBiome], slope: Int): Long = {
    val filtered = biomes.zipWithIndex.collect {
      case (b, i) if i % slope == 0 => b
    }

    xSlope(filtered, 1)
  }
}

sealed trait Biome

case object Tree extends Biome
case object Open extends Biome

case class RepeatedBiome(biomes: IndexedSeq[Biome]) {
  private val size = biomes.size
  def at(i: Int) = {
    val mod = i % size
    biomes(mod)
  }
}

object RepeatedBiome {
  def fromString(s: String): RepeatedBiome = {
    val biomes = s.map {
      case '.' => Open
      case '#' => Tree
    }
    RepeatedBiome(biomes)
  }
}
