import Part.*

case class Race(time: Long, record: Long)

object Day6:

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def solution(time: Long, distance: Long): Long =

    val(t, d) = (time.toDouble, distance.toDouble)
    val discriminant = Math.sqrt((t * t) - (4 * d))
    val(firstRoot, secondRoot) = ((t - discriminant) / 2, (t + discriminant) / 2)

    val firstRootCeiled = firstRoot.ceil.toLong
    val secondRootFloored = secondRoot.floor.toLong

    val lowerBound = if firstRootCeiled == firstRoot then firstRootCeiled + 1L else firstRootCeiled
    val upperBound = if secondRootFloored == secondRoot then secondRootFloored - 1L else secondRootFloored

    upperBound - lowerBound + 1L

  def part1(lines: List[String]): Int =
    val races = getRacesPart1(lines)
    val result = races
      .map(race => solution(race.time, race.record))
      .product

    Utils.printResult(Part1, result.toString)
    result.toInt

  def part2(lines:List[String]): Int =
    val race = Race(parseLinePart2(lines.head), parseLinePart2(lines(1)))
    val result = solution(race.time, race.record)
    Utils.printResult(Part2, result.toString)
    result.toInt

  def getRacesPart1(lines: List[String]): Seq[Race] =
    val (times, distances) = (parseLinePart1(lines.head), parseLinePart1(lines.last))
    times.zip(distances).map((t,d) => Race(t,d))

  def getRacesPart2(lines: List[String]): Race =
    Race(parseLinePart2(lines.head), parseLinePart2(lines.last))

  def parseLinePart1(line: String): Vector[Long] =
    line match
      case s"Time: $x" => x.split(" ").filter(_.nonEmpty).map(_.toLong).toVector
      case s"Distance: $x" => x.split(" ").filter(_.nonEmpty).map(_.toLong).toVector

  def parseLinePart2(line:String): Long =
    line match
      case s"Time: $x" => x.filterNot(_.isSpaceChar).toLong
      case s"Distance: $x" => x.filterNot(_.isSpaceChar).toLong