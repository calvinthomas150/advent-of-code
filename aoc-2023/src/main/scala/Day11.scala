import Part.*

object Day11:
  type Universe = List[String]
  type Coordinate = (Int, Int)

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def part1(lines: List[String]): Long =
    val result = run(lines, 2)
    Utils.printResult(Part1, result.toString)
    result

  def run(universe: Universe, expansionFactor: Int): Long =
    val galaxyCoordinates = getGalaxyCoordinates(universe)
    val galaxyPairs = getGalaxyPairs(galaxyCoordinates)
    val expansionRows = getExpansionRows(universe)
    val expansionColumns = getExpansionColumns(universe)
    
    galaxyPairs.map((c1, c2) =>
      val (ec1, ec2) = expandCoordinates(c1, c2, expansionRows, expansionColumns, expansionFactor)
      manhattanDistance(ec1, ec2)).sum

  def part2(lines: List[String]): Long =
    val result = run(lines, 1_000_000)
    Utils.printResult(Part1, result.toString)
    result

  def getGalaxyPairs(coordinates: Seq[Coordinate]): Seq[(Coordinate, Coordinate)] =
    for (c1 <- coordinates.indices;
         c2 <- coordinates.indices if c1 < c2) yield (coordinates(c1), coordinates(c2))

  def getGalaxyCoordinates(universe: Universe): Seq[Coordinate] =
    for (x <- universe.indices;
         y <- universe(x).indices if universe(x)(y) == '#') yield (x, y)

  def expandCoordinates(
    first: Coordinate,
    second: Coordinate,
    expansionRows: List[Int],
    expansionColumns:List[Int],
    expansionFactor: Int): (Coordinate, Coordinate) =

    val (x1, y1) = first
    val (x2, y2) = second

    val expandedX1 = (expansionRows.count(_ < x1) * (expansionFactor - 1)) + x1
    val expandedX2 = (expansionRows.count(_ < x2) * (expansionFactor - 1)) + x2
    val expandedY1 = (expansionColumns.count(_ < y1) * (expansionFactor - 1)) + y1
    val expandedY2 = (expansionColumns.count(_ < y2) * (expansionFactor - 1)) + y2

    val expandedFirst = (expandedX1, expandedY1)
    val expandedSecond = (expandedX2, expandedY2)
    (expandedFirst, expandedSecond)

  def manhattanDistance(first: Coordinate, second: Coordinate): Long =
    (first._1 - second._1).abs + (first._2 - second._2).abs

  def getExpansionRows(universe: Universe): List[Int] =
    getExpansions(universe)

  def getExpansionColumns(universe: Universe): List[Int] =
    getExpansions(universe.transpose.map(_.mkString("")))

  def getExpansions(universe: Universe): List[Int] =
    val expansions = universe.map(_.forall(x => x == '.'))
    expansions
      .zipWithIndex
      .filter(_._1 == true)
      .map(_._2)