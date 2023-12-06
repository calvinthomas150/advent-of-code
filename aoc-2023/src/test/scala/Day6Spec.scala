import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day6Spec extends AnyFlatSpec with should.Matchers:

  "parseLinesPart1" should "parse each line and return an Array of Ints" in:
    val times = "Time:      7  15   30"
    val distances = "Distance:  9  40  200"

    Day6.parseLinePart1(times) should be(Array(7,15,30))
    Day6.parseLinePart1(distances) should be(Array(9,40,200))

  "getRacesPart1" should "parse the input and return a list of races" in:
    val input = Utils.getInputLines("test", 6).toList
    Day6.getRacesPart1(input) should be (List(Race(7,9), Race(15,40), Race(30, 200)))

  "solution" should "return the number of valid solutions for the time and distance provided" in:
    Day6.solution(7, 9) should be(4)
    Day6.solution(15, 40) should be(8)
    Day6.solution(30, 200) should be(9)
    Day6.solution(71530, 940200) should be(71503)

  "part1" should "return the correct result for the sample test input" in :
    val input = Utils.getInputLines("test", 6).toList
    Day6.part1(input) should be(288)

  "parseLinesPart2" should "parse each line and return the time or distance as longs" in :
    val times = "Time:      7  15   30"
    val distances = "Distance:  9  40  200"

    Day6.parseLinePart2(times) should be(71530L)
    Day6.parseLinePart2(distances) should be(940200L)

  "getRacesPart2" should "parse the input and return a list of races" in :
    val input = Utils.getInputLines("test", 6).toList
    Day6.getRacesPart2(input) should be(Race(71530, 940200))

  "part2" should "return the correct result for the sample test input" in :
    val input = Utils.getInputLines("test", 6).toList
    Day6.part2(input) should be(71503)