import Day7.Rank.{FIVE_OF_A_KIND, FOUR_OF_A_KIND, FULL_HOUSE, HIGH_CARD, ONE_PAIR, THREE_OF_A_KIND, TWO_PAIR}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day7Spec extends AnyFlatSpec with should.Matchers:
  "parse" should "split a line into a Hand and a Bet" in:
    Day7.parse("32T3K 765") should be(List('3','2','T','3','K'), 765)

  "getRank" should "return the correct rank for a given hand" in:
    Day7.getRank(List('Q', 'J', 'K', '7', '8')) should be(HIGH_CARD)
    Day7.getRank(List('3', '2', 'T', '3', 'K')) should be(ONE_PAIR)
    Day7.getRank(List('K', 'K', '6', '7', '7')) should be(TWO_PAIR)
    Day7.getRank(List('K', 'Q', '7', '7', '7')) should be(THREE_OF_A_KIND)
    Day7.getRank(List('Q', 'Q', 'Q', '7', '7')) should be(FULL_HOUSE)
    Day7.getRank(List('Q', 'Q', 'Q', 'Q', '7')) should be(FOUR_OF_A_KIND)
    Day7.getRank(List('Q', 'Q', 'Q', 'Q', 'Q')) should be(FIVE_OF_A_KIND)

  "part1" should "return the result for part 1" in:
    val input = Utils.getInputLines("test", 7).toList
    Day7.part1(input) should be(6440)

  "part2" should "return the result for part 2" in :
    val input = Utils.getInputLines("test", 7).toList
    Day7.part2(input) should be(5905)

