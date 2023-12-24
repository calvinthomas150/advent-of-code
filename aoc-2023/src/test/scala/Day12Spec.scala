import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day12Spec extends AnyFlatSpec with should.Matchers:
  val input: List[String] = Utils.getInputLines("test", 12).toList
    
  "part1" should "return the correct result" in:
    Day12.part1(input) should be(21)
    
