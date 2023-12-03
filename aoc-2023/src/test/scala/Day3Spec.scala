import Day3.{PartPosition, part1}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day3Spec  extends AnyFlatSpec with should.Matchers:

  "getSymbolCoordinates" should "return the coordinates (x,y) for the position of symbols in a line" in:
    val line = "...$.*.43.."
    val lineNumber = 2
    val expected = List((2,3), (2,5))
    Day3.getSymbolCoordinates(2, line, "[^0-9.]") should be(expected)

  "getPart" should "return the part position based on being somewhere in the number" in :
    val line = "...$.*.743456....."
    val maxCoordinate = (0, 17)
    val partOfNumber = (0,8)
    Day3.getPart(line, partOfNumber, maxCoordinate) should be(PartPosition(0,7,12))

  "getPartsFromSymbol" should "return the PartPosition of all the parts that are adjacent to the symbol" in:
    val lines =
      List( "467..114..",
            "...*......",
            "..35..633.",
            "......#...",
            "617*......",
            ".....+.58.",
            "..592.....",
            "......755.",
            "...$.*....",
            ".664.598..")

    Day3.getPartsFromSymbol((5,5), lines, (9,9)) should be (Set(PartPosition(6, 2, 4)))
    Day3.getPartsFromSymbol((1,3), lines, (9,9)) should be (Set(PartPosition(0, 0, 2), PartPosition(2,2,3)))
    Day3.getPartsFromSymbol((4,3), lines, (9,9)) should be (Set(PartPosition(4, 0, 2)))

  "getAllParts" should "return all parts based on the list of symbols provided" in:
    val lines =
      List("467..114..",
        "...*......",
        "..35..633.",
        "......#...")

    val symbols = List((1, 3), (3,6))
    val maxCoordinates = (3,9)

    Day3.getAllParts(lines, symbols, maxCoordinates) should be(List(PartPosition(0,0,2), PartPosition(2,2,3), PartPosition(2,6,8)))

  "getNumberFromPartPosition" should "return the number as an integer based on the provided part position" in:
    val lines =
      List("467..114..",
        "...*......",
        "..35..633.",
        "......#...")

    val partPosition = PartPosition(2,6,8)

    Day3.getNumberFromPartPosition(lines, partPosition) should be(633)

  "part1" should "return the sum of all valid parts" in :
    val lines =
      List("467..114..",
        "...*......",
        "..35..633.",
        "......#...",
        "617*......",
        ".....+.58.",
        "..592.....",
        "......755.",
        "...$.*....",
        ".664.598..")

    part1(lines) should be(4361)

  "isPartAdjacentToSymbol" should "return true if the part is touching a symbol (including diagonals) or false otherwise" in:
    val maxCoordinate = (9,9)

    val symbol = (8, 5)
    val partPosition = PartPosition(7, 6, 8)
    Day3.isPartAdjacentToSymbol(symbol, partPosition, maxCoordinate) should be(true)

    val symbol1 = (3,3)
    val partPosition1 = PartPosition(3, 0, 2)
    Day3.isPartAdjacentToSymbol(symbol1, partPosition1, maxCoordinate) should be(true)

    val symbol2 = (3, 3)
    val partPosition2 = PartPosition(2, 2, 4)
    Day3.isPartAdjacentToSymbol(symbol2, partPosition2, maxCoordinate) should be(true)

    val symbol3 = (3, 3)
    val partPosition3 = PartPosition(4, 2, 2)
    Day3.isPartAdjacentToSymbol(symbol3, partPosition3, maxCoordinate) should be(true)

    val symbol4 = (3, 3)
    val partPosition4 = PartPosition(4, 4, 4)
    Day3.isPartAdjacentToSymbol(symbol4, partPosition4, maxCoordinate) should be(true)

    val symbol5 = (3, 3)
    val partPosition5 = PartPosition(4, 5, 5)
    Day3.isPartAdjacentToSymbol(symbol5, partPosition5, maxCoordinate) should be(false)

    val symbol6 = (3, 3)
    val partPosition6 = PartPosition(3, 5, 5)
    Day3.isPartAdjacentToSymbol(symbol6, partPosition6, maxCoordinate) should be(false)

  "findGears" should "find the gears in the system, i.e. where the symbol is adjacent to exactly two part positions" in:
    val symbols = List((1,3), (4,3), (8,5))
    val parts = List(PartPosition(0,0,2), PartPosition(2,2,3), PartPosition(4,0,2), PartPosition(7,6,8), PartPosition(9,5,7))
    val gears = Map((8,5) -> List(PartPosition(7,6,8), PartPosition(9,5,7)), (1,3) -> List(PartPosition(0,0,2), PartPosition(2,2,3)))
    val maxCoordinate = (9,9)

    Day3.findGears(symbols, parts, maxCoordinate) should be(gears)

  "part2" should "return the sum of the gear ratio" in:
    val lines =
      List("467..114..",
        "...*......",
        "..35..633.",
        "......#...",
        "617*......",
        ".....+.58.",
        "..592.....",
        "......755.",
        "...$.*....",
        ".664.598..")

    Day3.part2(lines) should be(467835)

