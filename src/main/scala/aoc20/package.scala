package object aoc20 {

  def readInputLines(day: Int) =
    InputStreams.wholeInputForDayAsLines(day).compile.toVector

  def readInputLongs(day: Int) =
    readInputLines(day).map(_.map(_.toLong))

}
