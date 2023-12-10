import Day10.Grid
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import Day10.Pipe.*

import scala.io.Source

class Day10Spec extends AnyFlatSpec with should.Matchers:

  val lines: List[String] = Utils.getInputLines("test", 10).toList
  
  "parse" should "convert the text input into a Grid" in:

    val lines = Source.fromResource(s"test-puzzle-input/day10simple.txt").getLines.toList

    val expectedGrid =
      Grid(Vector(
        Vector(NONE, NONE, NONE, NONE, NONE),
        Vector(NONE, START, LEFTRIGHT, DOWNLEFT, NONE),
        Vector(NONE, UPDOWN, NONE, UPDOWN, NONE),
        Vector(NONE, UPRIGHT, LEFTRIGHT, UPLEFT, NONE),
        Vector(NONE, NONE, NONE, NONE, NONE)))

    Day10.parse(lines) should be(expectedGrid)

  "findStartPosition" should "return the location of S as a coordinate" in:
    val grid = Day10.parse(lines)
    Day10.findStartPosition(grid) should be(2, 0)
  
  "findConnectingPipeCoodinates" should "return the pipe coordinates that connect to the given coordinate" in:
    val lines = Source.fromResource(s"test-puzzle-input/day10simple.txt").getLines.toList
    val grid = Day10.parse(lines)
    val startCoordinate = Day10.findStartPosition(grid)
    Day10.getNeighbourCoordinates(grid, startCoordinate) should be(Vector((0, 1), (2, 1), (1, 0), (1, 2)))
    
  "part1" should "return the coorect value" in:
    Day10.part1(lines) should be(8)
