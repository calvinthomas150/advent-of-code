import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day11Spec extends AnyFlatSpec with should.Matchers:
  val input: List[String] = Utils.getInputLines("test", 11).toList
  
  "getExpansionRows" should "get the index of each row that should expand" in:
    Day11.getExpansionRows(input) should be(List(3,7))
  
  "getExpansionColumns" should "get the index of each column that should expand" in :
    Day11.getExpansionColumns(input) should be(List(2, 5, 8))

  "getGalaxyCoordinates" should "return a List of all the coordinates of galaxies" in :
    val input = Utils.getInputLines("test", 11).toList
    val universe = input
    Day11.getGalaxyCoordinates(universe) should be(Vector((0, 3), (1, 7), (2, 0), (4, 6), (5, 1), (6, 9), (8, 7), (9, 0), (9, 4)))

  "expandCoordinates" should "expand the coordinates based on the galaxy expansion rule" in:
    val first = (0,3)
    val second = (8,7)
    val expandedRows = List(3, 7)
    val expandedColumns = List(2, 5, 8)
    val expansionFactor = 2
    Day11.expandCoordinates(first, second, expandedRows, expandedColumns, expansionFactor) should be (((0, 4), (10, 9)))
  
  "manhattanDistance" should "return the correct distance between two galaxies" in :
    val first = (6, 1)
    val second = (11, 5)
    Day11.manhattanDistance(first, second) should be(9)

    val first2 = (0, 4)
    val second2 = (10, 9)
    Day11.manhattanDistance(first2, second2) should be(15)

  "getGalaxyPairs" should "return a sequence of all coordinate pairs, pairs should be treated the same if they're in the opposite order, i.e. (2,1) == (1,2)" in :
    val coordinates = List((1, 2), (3, 4), (5, 6), (7, 8))
    val pairs = List(((1, 2), (3, 4)), ((1, 2), (5, 6)), ((1, 2), (7, 8)), ((3, 4), (5, 6)), ((3, 4), (7, 8)), ((5, 6), (7, 8)))

    Day11.getGalaxyPairs(coordinates) should be(pairs)
    
  "part1" should "return the correct sum of galaxy pairs when the expansion factor is 2" in:
    Day11.part1(input) should be(374)

  "part2" should "return the correct sum of galaxy pairs when the expansion factor is 1,000,000" in :
    Day11.part2(input) should be(82000210)

