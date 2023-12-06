import Day6.Race
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day6Spec extends AnyFlatSpec with should.Matchers:
  "getRacesPart1" should "parse the input and return a list of races" in:
    val input = Utils.getInputLines("test", 6).toList
    Day6.getRacesPart1(input) should be (List(Race(7,9), Race(15,40), Race(30, 200)))

  "part1" should "return the correct result" in :
    val input = Utils.getInputLines("test", 6).toList
    Day6.part1(input) should be(288)

  "getRacesPart2" should "parse the input and return a list of races" in :
    val input = Utils.getInputLines("test", 6).toList
    Day6.getRacesPart2(input) should be(Race(71530, 940200))

  "part2" should "return the correct result" in :
    val input = Utils.getInputLines("test", 6).toList
    Day6.part2(input) should be(71503)