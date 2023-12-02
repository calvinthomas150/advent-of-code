import Part.*

object Day1:

  def run(mode:String, dayNumber: Int): Unit =
    val linesPart1 = Utils.getInputLines(mode, dayNumber)
    part1(linesPart1)
    val linesPart2 = Utils.getInputLines(mode, dayNumber)
    part2(linesPart2)

  def part1(lines: Iterator[String]): Unit =
    val numbersOnly = removeAllNonNumericCharacters(lines)
    val firstAndLast = firstAndLastNumbers(numbersOnly)
    val result = combineAndSumFirstAndLast(firstAndLast)
    Utils.printResult(Part1, result.toString)

  def part2(lines: Iterator[String]): Unit =
    val numbersOnly = removeAllNonNumericWithWordsCharacters(lines)
    val firstAndLast = firstAndLastNumbers(numbersOnly)
    val result = combineAndSumFirstAndLast(firstAndLast)
    Utils.printResult(Part2, result.toString)

  def removeAllNonNumericCharacters(lines:Iterator[String]):Iterator[String] =
    lines.map(_.replaceAll("[^0-9]", ""))

  def removeAllNonNumericWithWordsCharacters(lines: Iterator[String]): Iterator[String] =

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
          case (currentStr, (word, number)) =>
            currentStr.replaceAll(word, number)

        result.replaceAll("[^0-9]", "")


  def firstAndLastNumbers(numbersOnly: Iterator[String]): Iterator[(Int, Int)] =
    numbersOnly.map(n => (n.head.asDigit,n.last.asDigit))

  def combineAndSumFirstAndLast(nums: Iterator[(Int, Int)]): Int =
    nums.map((x, y) => s"$x$y".toInt).sum

