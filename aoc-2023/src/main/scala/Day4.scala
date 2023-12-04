import Part.*

import scala.annotation.tailrec
import scala.collection.mutable

object Day4 {

  case class Scratchcard(id: Int, winningNumbers: List[Int], cardNumbers: List[Int], isCopy: Boolean = false)

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def part1(lines: List[String]):BigInt =
    val scratchcards = getScratchcards(lines)
    val result = scratchcards.map(getPoints).sum
    Utils.printResult(Part1, result.toString)
    result

  def part2(lines: List[String]): Int =
    val scratchcards = getScratchcards(lines).toList

    val result = scratchcardMapWithCopies(scratchcards).values.sum

    Utils.printResult(Part2, result.toString)
    result


  def scratchcardMapWithCopies(scratchcards: List[Scratchcard]): Map[Int, Int] =
    val startingMap = (for(card <- scratchcards) yield card.id -> 1).toMap

    val updatedMap =
      scratchcards.foldLeft(startingMap)((currentMap, scratchcard) =>
        val numberOfWinners = countWinningNumbers(scratchcard)
        getUpdatedMap(scratchcard.id, numberOfWinners, currentMap))

    updatedMap

  def getUpdatedMap(id: Int, numberOfWinners: Int, currentMap: Map[Int, Int]): Map[Int, Int] =
    val updated = (id + 1  to id + numberOfWinners).foldLeft(currentMap)((currentMap, updateId) =>
      currentMap.updated(updateId, currentMap(id) + currentMap(updateId)))

    updated

  def countWinningNumbers(card: Scratchcard): Int =
    (card.winningNumbers intersect card.cardNumbers).size

  def getPoints(card: Scratchcard):BigInt =
    val winningNumbers = countWinningNumbers(card: Scratchcard)
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
