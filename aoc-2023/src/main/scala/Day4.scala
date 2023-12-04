import Part.*

object Day4 {

  case class Scratchcard(id: Int, winningNumbers: List[Int], cardNumbers: List[Int])

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)

  def part1(lines: List[String]):BigInt =
    val scratchcards = getScratchcards(lines)
    val result = scratchcards.map(getPoints).sum
    println(result)
    Utils.printResult(Part1, result.toString)
    result

  def getPoints(card: Scratchcard):BigInt =
    val winningNumbers = (card.winningNumbers intersect card.cardNumbers).size
    BigDecimal.valueOf(Math.pow(2, winningNumbers - 1)).toBigInt

  def getScratchcards(lines: List[String]): Seq[Scratchcard] =
    lines map getScratchcard

  def getScratchcard(line: String):Scratchcard =
    val id:Int = line.dropWhile(!_.isDigit).takeWhile(_ != ':').toInt
    val numbers = line.dropWhile(_ != ':').drop(1).split("\\|").toList
    val (winningNumbersString:String, cardNumbersString:String) = (numbers.head.trim, numbers.tail.head.trim)
    val winningNumbers = convertNumbersInput(winningNumbersString)
    val cardNumbers = convertNumbersInput(cardNumbersString)

    Scratchcard(id, winningNumbers, cardNumbers)

  def convertNumbersInput(numberString: String): List[Int] =
    numberString
      .trim
      .split(" ")
      .filter(_.matches("\\d+"))
      .map(_.toInt)
      .toList
}
