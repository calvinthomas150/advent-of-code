import Part.*

object Day6:

  case class Race(time: Long, record: Long)

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def solution(time: Long, distance: Long): Long =
    val(t, d) = (time.toDouble, distance.toDouble)
    val discriminant = Math.sqrt((t * t) - (4 * d))
    val(rt1, rt2) = ((t / 2) - (discriminant / 2), (t / 2) + (discriminant / 2))

    val rt1Ceil = rt1.ceil.toLong
    val rt2Floor = rt2.floor.toLong

    val lowerBound = if rt1Ceil == rt1 then rt1Ceil + 1L else rt1Ceil
    val upperBound = if rt2Floor == rt2 then rt2Floor - 1L else rt2Floor

    upperBound - lowerBound + 1L

  def part1(lines: List[String]): Int =
    val races = getRacesPart1(lines)
    val result = races
      .map(race => solution(race.time, race.record))
      .product

    Utils.printResult(Part1, result.toString)
    result.toInt

  def part2(lines:List[String]): Int =
    val race = getRacesPart2(lines)
    val result = solution(race.time, race.record)

    Utils.printResult(Part2, result.toString)
    result.toInt

  def getRacesPart1(lines: List[String]): Seq[Race] =
    val List(t, d) = lines
    val times = parseLinePart1(t)
    val distance = parseLinePart1(d)
    times.zip(distance).map((t,d) => Race(t,d))

  def parseLinePart1(line: String): Seq[Int] =
    line
      .dropWhile(!_.isDigit)
      .split("\\s+")
      .map(_.toInt)
      .toList

  def getRacesPart2(lines: List[String]): Race =
    val List(t, d) = lines
    val time = parseLinePart2(t)
    val distance = parseLinePart2(d)
    Race(time, distance)

  def parseLinePart2(line:String): Long =
    line
      .dropWhile(!_.isDigit)
      .replaceAll("[^0-9]", "")
      .toLong