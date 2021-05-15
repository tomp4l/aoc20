import cats.effect.IO
package object aoc20:

  def readInput(day: Int) =
    InputStreams.wholeInputForDay(day).compile.foldMonoid

  def readInputLines(day: Int) =
    InputStreams.wholeInputForDayAsLines(day).compile.toVector

  def readInputLongs(day: Int) =
    readInputLines(day).map(_.map(_.toLong))

  extension [A](io: IO[A])
    def time(tag: String): IO[A] = for
      startTime <- IO(System.nanoTime)
      v <- io
      endTime <- IO(System.nanoTime)
      seconds = (endTime - startTime).toDouble / 1000000000d
      _ <- Console.writeLine(s"IO $tag took ${seconds}s")
    yield v
  extension (s: String)
    def indexOfOption(ch: Int) = s.indexOf(ch) match
      case -1 => None
      case i => Some(i)
