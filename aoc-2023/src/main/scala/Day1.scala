import Part.*

object Day1:

  def run(mode:String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def part1(lines: List[String]): Unit =
    val result = solve(lines, removeAllNonNumericCharacters)
    Utils.printResult(Part1, result.toString)

  def part2(lines: List[String]): Unit =
    val result = solve(lines, removeAllNonNumericWithWordsCharacters)
    Utils.printResult(Part2, result.toString)

  def solve(lines: List[String], parse: List[String] => List[String]): Int =
    val numbersOnly = parse(lines)
    val firstAndLast = firstAndLastNumbers(numbersOnly)
    combineAndSumFirstAndLast(firstAndLast)

  def removeAllNonNumericCharacters(lines:List[String]):List[String] =
    lines.map(_.replaceAll("[^0-9]", ""))

  def removeAllNonNumericWithWordsCharacters(lines: List[String]): List[String] =

    val numberMap = Map(
      "one" -> "1",
      "two" -> "2",
      "three" -> "3",
      "four" -> "4",
      "five" -> "5",
      "six" -> "6",
      "seven" -> "7",
      "eight" -> "8",
      "nine" -> "9"
    )

    lines.map:
      line =>
        val result = numberMap.foldLeft(line):
          case (currentStr, (word, number)) => currentStr.replaceAll(word, number)
        result.replaceAll("[^0-9]", "")

  def firstAndLastNumbers(numbersOnly: List[String]): List[(Int, Int)] =
    numbersOnly.map(n => (n.head.asDigit,n.last.asDigit))

  def combineAndSumFirstAndLast(nums: List[(Int, Int)]): Int =
    nums.map((x, y) => s"$x$y".toInt).sum

