import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day13Spec extends AnyFlatSpec with should.Matchers:
  val input: String = Day13.getInput("test", 13)
  
  "getPatterns" should "split the input into each pattern" in:
    val firstPattern = "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#."
    Day13.getPatterns(input).head should be(firstPattern)
    
  "getReflectionPoint" should "find the index of the reflection point when it exists" in:
    val secondPattern = Day13.getPatterns(input).tail.head
    Day13.findReflectionPoint(secondPattern) should be(Some(3))

  "getVerticalReflectionPoint" should "find the index of the reflection point when it exists" in :
    val firstPattern = Day13.getPatterns(input).head
    Day13.findReflectionPoint(firstPattern) should be(Some(4))

  "part1" should "give the correct result for part1" in :
    Day13.part1(Day13.getPatterns(input)) should be(405)

  "part2" should "give the correct result for part2" in :
    Day13.part2(Day13.getPatterns(input)) should be(400)
    
