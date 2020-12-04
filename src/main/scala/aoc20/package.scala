package object aoc20 {

  def readInput(day: Int) =
    InputStreams.wholeInputForDay(day).compile.foldMonoid

  def readInputLines(day: Int) =
    InputStreams.wholeInputForDayAsLines(day).compile.toVector

  def readInputLongs(day: Int) =
    readInputLines(day).map(_.map(_.toLong))

}
