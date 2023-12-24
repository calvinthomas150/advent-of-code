import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day12Spec extends AnyFlatSpec with should.Matchers:
  val input: List[String] = Utils.getInputLines("test", 12).toList
  
  "parse" should "split the input into Srings and damaged groups" in:
    val parsed: List[(List[Char], Array[Int])] =
      List((List('?', '?', '?', '.', '#', '#', '#'), Array(1, 1, 3)))
    
    Day12.parse(input).head :: Nil == parsed
    
  "createPossibleSpringCombinations" should "return all possible combinations of working and damaged springs" in:
    val springs = Day12.parse(input).head._1

    val combinations = 
      List(
        List('.', '.', '.', '.', '#', '#', '#'), 
        List('#', '.', '.', '.', '#', '#', '#'), 
        List('.', '#', '.', '.', '#', '#', '#'), 
        List('#', '#', '.', '.', '#', '#', '#'), 
        List('.', '.', '#', '.', '#', '#', '#'), 
        List('#', '.', '#', '.', '#', '#', '#'), 
        List('.', '#', '#', '.', '#', '#', '#'), 
        List('#', '#', '#', '.', '#', '#', '#'))

    Day12.createPossibleSpringCombinations(springs) should be(combinations)
  
  "countGroups" should "return a count of the groups of damaged springs" in:
    val springs = List('#', '.', '#', '.', '#', '#', '#')
    Day12.countGroups(springs) should be(List(1,1,3))
    Day12.countGroups(
      List('.', '#', '#', '.', '#', '#', '#')
    ) should be(List(2,3))
    
  "part1" should "return the correct result" in:
    Day12.part1(input) should be(21)
    
