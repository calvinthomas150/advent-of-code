import Part.*

object Day3:

  type Coordinate = (Int, Int)
  case class PartPosition(lineNumber: Int, start: Int, end: Int)
  val numericRegex = "[0-9]"

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def part1(lines:List[String]): Int =
    val maxX = lines.length - 1
    val maxY = lines.head.length - 1
    given maxCoordinate: Coordinate = (maxX, maxY)
    val linesWithNumber = lines zip (0 to maxX)

    val symbols = for((line, lineNumber) <- linesWithNumber) yield getSymbolCoordinates(lineNumber, line, "[^0-9.]")
    val partCoordinates = getAllParts(lines, symbols.flatten)
    val result = partCoordinates.map(getNumberFromPartPosition(lines, _)).sum

    Utils.printResult(Part1, result.toString)
    result

  def part2(lines: List[String]): Long =
    val maxX = lines.length - 1
    val maxY = lines.head.length - 1
    given maxCoordinate: Coordinate = (maxX, maxY)
    val linesWithNumber = lines zip (0 to maxX)

    val symbols = for((line, lineNumber) <- linesWithNumber) yield getSymbolCoordinates(lineNumber, line, "\\*")
    val partCoordinates = getAllParts(lines, symbols.flatten)

    val gears = findGears(symbols.flatten, partCoordinates)

    val result =
      gears
        .view
        .mapValues(_.map(getNumberFromPartPosition(lines, _)))
        .mapValues(_.product)
        .values
        .sum

    Utils.printResult(Part2, result.toString)
    result

  def findGears(symbols: Seq[(Int, Int)], parts: List[Day3.PartPosition])(using maxCoordinate: Coordinate): Map[Coordinate, Seq[PartPosition]] =

    val symbolsAndParts =
      for( symbol <- symbols;
         part <- parts if isPartAdjacentToSymbol(symbol, part)) yield (symbol, part)

    val gears =
      symbolsAndParts
      .groupBy((symbol, _) => symbol)
      .filter((_, parts) => parts.size == 2)
      .view
      .mapValues(_.map((_, part) => part))
      .toMap
      
    gears

  def isPartAdjacentToSymbol(symbol: Coordinate, partPosition: PartPosition)(using maxCoordinate: Coordinate):Boolean =

    val (maxX, maxY) = maxCoordinate
    val (symbolX, symbolY) = symbol
    val xRange = Math.max(0, symbolX - 1) to Math.min(symbolX + 1, maxX)
    val yRange = Math.max(0, symbolY - 1) to Math.min(symbolY + 1, maxY)

    val xOverlaps = xRange.contains(partPosition.lineNumber)
    val partPositionRange = partPosition.start to partPosition.end
    val yOverlaps = partPositionRange.intersect(yRange).nonEmpty

    yOverlaps && xOverlaps

  def getSymbolCoordinates(lineNumber: Int, line: String, regex: String): Seq[(Int, Int)] =
    val (symbolCoordinates, _) =
      line.foldLeft((Nil:List[Coordinate], 0)):
        case ((coordinates, y) ,char) =>
          if(char.toString.matches(regex))
            ((lineNumber, y) :: coordinates, y + 1)
          else
            (coordinates, y + 1)

    symbolCoordinates.reverse

  def getAllParts(lines:List[String], symbols: List[Coordinate])(using maxCoordinate: Coordinate):List[PartPosition] =
    val parts = for(symbol <- symbols) yield getPartsFromSymbol(symbol, lines)
    parts.flatten

  def getPartsFromSymbol(symbol: Coordinate, lines:List[String])(using maxCoordinate: Coordinate): Set[PartPosition] =
    val (symbolX, symbolY) = symbol
    val (maxX, maxY) = maxCoordinate
    val xRange = Math.max(0, symbolX - 1) to Math.min(symbolX + 1, maxX)
    val yRange = Math.max(0, symbolY - 1) to Math.min(symbolY + 1, maxY)

    val partsWithDuplicates = for(x <- xRange;
        y <- yRange if lines(x)(y).toString.matches(numericRegex)
    ) yield getPart(lines(x), (x,y))

    partsWithDuplicates.toSet

  def getPart(line: String, partOfNumber: Coordinate)(implicit maxCoordinate: Coordinate): PartPosition =
    val (_, maxY) = maxCoordinate
    val (numberX, numberY) = partOfNumber
    val numberToStart = numberY to 0 by -1
    val numberToEnd = numberY to maxY

    val start = numberToStart.takeWhile(line.charAt(_).toString.matches(numericRegex)).min
    val end = numberToEnd.takeWhile(line.charAt(_).toString.matches(numericRegex)).max

    PartPosition(numberX, start, end)

  def getNumberFromPartPosition(lines: List[String], partPosition: PartPosition): Int =
      lines(partPosition.lineNumber).substring(partPosition.start, partPosition.end + 1).toInt



