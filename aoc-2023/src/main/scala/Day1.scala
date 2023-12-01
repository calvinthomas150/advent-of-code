object Day1 {

  def run(mode:String, dayNumber: Int): Unit = {
    val linesPart1 = Utils.getInputLines(mode, dayNumber)
    part1(linesPart1)
    val linesPart2 = Utils.getInputLines(mode, dayNumber)
    part2(linesPart2)
  }

  def part1(lines: Iterator[String]): Unit = {
    val numbersOnly = removeAllNonNumericCharacters(lines)
    val firstAndLast = firstAndLastNumbers(numbersOnly)
    val result = combineAndSumFirstAndLast(firstAndLast)
    println(s"Part 1 Result: $result")
  }

  def part2(lines: Iterator[String]): Unit = {
    val numbersOnly = removeAllNonNumericWithWordsCharacters(lines)
    val firstAndLast = firstAndLastNumbers(numbersOnly)
    val result = combineAndSumFirstAndLast(firstAndLast)
    println(s"Part 2 Result: $result")
  }

  def removeAllNonNumericCharacters(lines:Iterator[String]):Iterator[String] = {
    lines.map(_.replaceAll("[^0-9]", ""))
  }

  def removeAllNonNumericWithWordsCharacters(lines: Iterator[String]): Iterator[String] = {
      lines
        .map(_.replaceAll("one", "one1one")
              .replaceAll("two", "two2two")
              .replaceAll("three", "three3three")
              .replaceAll("four", "four4four")
              .replaceAll("five", "five5five")
              .replaceAll("six", "six6six")
              .replaceAll("seven", "seven7seven")
              .replaceAll("eight", "eight8eight")
              .replaceAll("nine", "nine9nine")
              .replaceAll("[^0-9]", ""))
  }

  def firstAndLastNumbers(numbersOnly: Iterator[String]): Iterator[(Int, Int)] = {
    numbersOnly.map(n => {
      (n.head.asDigit,n.last.asDigit)
    })
  }

  def combineAndSumFirstAndLast(nums: Iterator[(Int, Int)]): Int = {
    nums.map((x, y) => s"$x$y".toInt).sum
  }
}
