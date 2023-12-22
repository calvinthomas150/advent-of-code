import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day11Spec extends AnyFlatSpec with should.Matchers:

  given expansionCount: Int = 2
  
  "addExtraRows" should "add extra rows where there are no galaxies" in:
    val input = Utils.getInputLines("test", 11).toList
    val expected =
      List( "...#......",
            ".......#..",
            "#.........",
            "..........",
            "..........",
            "......#...",
            ".#........",
            ".........#",
            "..........",
            "..........",
            ".......#..",
            "#...#.....")

    Day11.addExtraRows(input) should be(expected)

  "addExtraColumns" should "add extra columns where there are no galaxies" in :
    val input = Utils.getInputLines("test", 11).toList
    val expected =
      List( "....#........",
            ".........#...",
            "#............",
            ".............",
            "........#....",
            ".#...........",
            "............#",
            ".............",
            ".........#...",
            "#....#.......")

    Day11.addExtraColumns(input) should be(expected)

  "parse" should "add extra rows and columns where there are no galaxies" in:
    val input = Utils.getInputLines("test", 11).toList
    val expected =
      List( "....#........",
            ".........#...",
            "#............",
            ".............",
            ".............",
            "........#....",
            ".#...........",
            "............#",
            ".............",
            ".............",
            ".........#...",
            "#....#.......")

    Day11.parse(input) should be(expected)

  "getGalaxyCoordingates" should "return a List of all the coordingates of galaxies" in:
    val input = Utils.getInputLines("test", 11).toList
    val universe = Day11.parse(input)
    Day11.getGalaxyCoordinates(universe) should be (List((0, 4), (1, 9), (2, 0), (5, 8), (6, 1), (7, 12), (10, 9), (11, 0), (11, 5)))

  "calculateCoordinateDistance" should "return the correct distance between two galaxies" in:
    val first = (6,1)
    val second = (11, 5)
    Day11.calculateCoordinateDistance(first, second) should be(9)

    val first2 = (0,4)
    val second2 = (10, 9)
    Day11.calculateCoordinateDistance(first2, second2) should be (15)
    
  "getGalaxyPairs" should "return a sequence of all coordinate pairs, pairs should be treated the same if they're in the opposite order, i.e. (2,1) == (1,2)" in:
    val coordinates = List((1,2), (3,4), (5,6), (7,8))
    val pairs = List(((1,2), (3,4)), ((1,2), (5,6)), ((1,2), (7,8)), ((3,4), (5,6)), ((3,4), (7,8)),((5,6), (7,8)))
    
    Day11.getGalaxyPairs(coordinates) should be(pairs)
    
  "part1" should "calculate the shortest path between all pairs of galaxies" in:
    val input = Utils.getInputLines("test", 11).toList
    Day11.part1(input) should be(374)

  "part2" should "calculate the shortest path between all pairs of galaxies with larger expansion" in :
    val input = Utils.getInputLines("test", 11).toList
    Day11.part2(input) should be(374)

