package aoc20
package day20

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.implicits._

import scala.util.chaining._

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    readInput(20)
      .flatMap { input =>
        val tileString = input.split("\n\n")
        val tiles = tileString.flatMap(Tile.fromString).toVector
        for {
          _ <- Console.output(
            1,
            FullImage.findCorners(tiles).map(_.id.toLong).product,
          )
          _ <- Console.output(
            2, {
              val image = FullImage.fromTiles(tiles)
              val rotations: Seq[FullImage => FullImage] =
                Seq(identity, _.rotate, _.rotate.rotate, _.rotate.rotate)
              val flips: Seq[FullImage => FullImage] = Seq(identity, _.flip)
              val allImages =
                (rotations, flips)
                  .mapN { case (a, b) => a andThen b }
                  .map(_.apply(image))
              allImages.map(_.roughness).min
            },
          )
        } yield ()
      }
      .as(ExitCode.Success)

}

case class Tile(id: Int, pixels: Seq[Seq[Boolean]]) {
  def asString = s"Tile ($id):\n" ++ Pixels.asString(pixels)

  private def boolsToInt(bools: Seq[Boolean]): Int =
    bools.map(if (_) '1' else "0").mkString.pipe(Integer.parseInt(_, 2))

  val top = boolsToInt(pixels.head)
  val bottom = boolsToInt(pixels.last)
  val left = boolsToInt(pixels.map(_.head))
  val right = boolsToInt(pixels.map(_.last))

  val sides: Seq[Int] =
    Seq(
      top,
      right,
      bottom,
      left,
    )

  def rotate: Tile = copy(pixels = Pixels.rotate(pixels))
  def flip: Tile = copy(pixels = Pixels.flip(pixels))

  def neighbours(side: Int): Boolean =
    sides.contains(side) || rotate.rotate.sides.contains(side)
}

object Tile {
  def fromString(s: String) = {
    val split = s.split("\n")
    for {
      h <- split.headOption
      m <- """Tile (\d+):""".r.findFirstMatchIn(h)
      id <- m.group(1).toIntOption
    } yield Tile(id, split.tail.toVector.map(_.toVector.map(_ == '#')))
  }
}

object Pixels {
  def rotate(pixels: Seq[Seq[Boolean]]) =
    pixels.foldLeft(Seq.fill(pixels.length)(Seq.empty[Boolean])) {
      (acc, line) =>
        acc.zip(line.reverse).map { case (s, b) => s :+ b }
    }

  def flip(pixels: Seq[Seq[Boolean]]) =
    pixels.foldLeft(Seq.fill(pixels.length)(Seq.empty[Boolean])) {
      (acc, line) =>
        acc.zip(line).map { case (s, b) => s :+ b }
    }

  def asString(pixels: Seq[Seq[Boolean]]) = pixels
    .map(v => v.map(if (_) '#' else '.').mkString ++ "\n")
    .mkString
}

case class FullImage(pixels: Seq[Seq[Boolean]]) {
  import FullImage._

  def asString = Pixels.asString(pixels)

  def rotate: FullImage = copy(pixels = Pixels.rotate(pixels))
  def flip: FullImage = copy(pixels = Pixels.flip(pixels))

  def findSeaMonsters = pixels
    .sliding(SeaMonsterHeight)
    .map { slices =>
      val patterns = slices.map(_.sliding(SeaMonsterWidth).toVector)
      val masks = patterns.zip(SeaMonsterMask).map { case (s, m) =>
        s.map(_.zip(m).forall { case (l, r) => if (r) l else true })
      }
      val grouped = masks.tail.foldLeft(masks.head.map(Seq(_))) { (a, b) =>
        a.zip(b).map { case (a, b) => a :+ b }
      }
      grouped.count(_.forall(identity))
    }
    .sum

  def roughness =
    pixels.map(_.count(_ == true)).sum - findSeaMonsters * SeaMonsterPixels
}

object FullImage {
  final private val SeaMonsterString =
    """                  # 
      |#    ##    ##    ###
      | #  #  #  #  #  #   """.stripMargin

  final private val SeaMonsterMask: Seq[Seq[Boolean]] =
    SeaMonsterString.split("\n").map(_.map(_ == '#').toVector).toVector

  final private val SeaMonsterHeight = SeaMonsterMask.length
  final private val SeaMonsterWidth = SeaMonsterMask.head.length

  final private val SeaMonsterPixels: Int =
    SeaMonsterMask.map(_.count(_ == true)).sum

  def findCorners(tiles: Seq[Tile]) =
    tiles.filter(findNeighbours(_, tiles).length == 2)

  private def findNeighbours(tile: Tile, tiles: Seq[Tile]) = {
    val rest = tiles.filterNot(_ == tile)
    val sides = tile.sides
    rest
      .filter(t =>
        (t.sides ++ t.rotate.rotate.sides)
          .intersect(sides)
          .nonEmpty,
      )
      .map(_.id)
  }

  private def along(
    compare: (Tile, Tile) => Boolean,
    next: (Tile) => Int,
  ): (Tile, Tile, Seq[Tile]) => Seq[Tile] = (a, b, remaining) => {
    val matched =
      if (compare(a, b)) Some(b)
      else if (compare(a, b.flip)) Some(b.flip)
      else None
    matched
      .map { m =>
        val newRemaining = remaining.filter(_.id != m.id)
        newRemaining
          .find(_.neighbours(next(m)))
          .map(r => a +: along(compare, next)(m, r, newRemaining))
          .getOrElse(Seq(a, m))
      }
      .getOrElse {
        along(compare, next)(a, b.rotate, remaining)
      }
  }

  private val row = along((a, b) => a.right == b.left, _.right)
  private val column = along((a, b) => a.bottom == b.top, _.bottom)

  def fromTiles(tiles: Seq[Tile]) = {
    val corner = findCorners(tiles).head
    val remaining = tiles.filter(_.id != corner.id)

    def orientTopLeft(corner: Tile): Tile = {
      val right = remaining.exists(t => t.neighbours(corner.right))
      val bottom = remaining.exists(t => t.neighbours(corner.bottom))
      if (right && bottom) corner
      else orientTopLeft(corner.rotate)
    }

    val topLeft = orientTopLeft(corner)

    val left = column(
      topLeft,
      remaining.find(_.neighbours(topLeft.bottom)).get,
      remaining,
    )

    val alignedTiles = left.map { t =>
      val r = remaining.filter(_.id != t.id)
      row(t, r.find(_.neighbours(t.right)).get, r)
    }

    FullImage(alignedTiles.flatMap { row =>
      val l = row.head.pixels.length
      row
        .map(_.pixels.tail.init)
        .foldRight(Seq.fill(l)(Seq.empty[Boolean])) { (a, b) =>
          a.zip(b).map { case (l, r) => l.tail.init ++ r }
        }
    })
  }
}
