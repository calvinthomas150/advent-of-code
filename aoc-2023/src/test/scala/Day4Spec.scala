import Day4.{Scratchcard, part1}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day4Spec extends AnyFlatSpec with should.Matchers:
  "getScratchcard" should "parse the input for a single line in to a scratchcard object" in:
    val input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    val expected = Scratchcard(1, List(41,48,83,86,17), List(83,86,6,31,17,9,48,53))
    Day4.getScratchcard(input) should be(expected)

  "convertNumberString" should "convert the strings from the input into a List of integers" in:
    val input1 = "41 48 83 86 17 "
    val expected1 = List(41,48,83,86,17)
    Day4.convertNumbersInput(input1) should be(expected1)

    val input2 = " 83 86  6 31 17  9 48 53"
    val expected2 = List(83,86,6,31,17,9,48,53)
    Day4.convertNumbersInput(input2) should be(expected2)

  "getPoints" should "return the correct number based on doubling for the amount of winning numbers matched" in:
    val winningNumbers = List(41,48,83,86,17)
    val cardNumbers = List(83,86,6,31,17,9,48,53)
    val scratchcard = Scratchcard(1, winningNumbers, cardNumbers)

    Day4.getPoints(scratchcard) should be(8)

  "part1" should "return the number of points from all card, calculated as a doubling of the matching on each card summed together" in:
    val input =
      List("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
           "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
           "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
           "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
           "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
           "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

    Day4.part1(input) should be(13)
