package aoc20

import fs2.{text, Stream}
import java.nio.file.Paths
import cats.effect.IO
import fs2.io.file.Files

object InputStreams {
  def wholeInputForDay(day: Int, chunkSize: Int = 4096): Stream[IO, String] = {
    val path = s"inputs/day$day.txt"
    Files[IO].readAll(Paths.get(path), chunkSize).through(text.utf8Decode)
  }

  def wholeInputForDayAsLines(
    day: Int,
    chunkSize: Int = 4096,
  ): Stream[IO, String] =
    wholeInputForDay(day, chunkSize).through(text.lines)
}
