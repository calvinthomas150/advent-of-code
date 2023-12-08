import Day7.Rank.*
import Day8.Direction
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day8Spec extends AnyFlatSpec with should.Matchers:

  val lines: Seq[String] = Utils.getInputLines("test", 8).toList

  "parseDirections" should "parse the input and return a List of Directions" in:
    Day8.parseDirections(lines.head) should be(List(Direction.RIGHT, Direction.LEFT))

  "parseNodes" should "parse the node information into a map from node to left and right" in:
    val expected =
      Map(
      "AAA" -> ("BBB", "CCC"),
      "BBB" -> ("DDD", "EEE"),
      "CCC" -> ("ZZZ", "GGG"),
      "DDD" -> ("DDD", "DDD"),
      "EEE" -> ("EEE", "EEE"),
      "GGG" -> ("GGG", "GGG"),
      "ZZZ" -> ("ZZZ", "ZZZ"),
    )
    Day8.parseNodes(lines) should be(expected)
    
  "part1" should "return the number of steps needed to move from AAA to ZZZ based on left right movement" in:
    Day8.part1(lines) should be(2)
    

