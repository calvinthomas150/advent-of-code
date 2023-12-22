import Part.Part1

object Day11:

  type Universe = List[String]
  type Coordinate = (Int, Int)

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def part1(lines: List[String]): Int =
    val result = run(lines, 2)
    Utils.printResult(Part1, result.toString)
    result

  def part2(lines: List[String]): Int =
    ???
  
  def run(lines: List[String], factor: Int): Int =
    given expansionFactor:Int = factor
    val universe = parse(lines)
    val galaxyCoordinates = getGalaxyCoordinates(universe)
    val galaxyPairs = getGalaxyPairs(galaxyCoordinates)
    galaxyPairs.map((c1, c2) => calculateCoordinateDistance(c1, c2)).sum

  def getGalaxyPairs(coordinates: Seq[Coordinate]):Seq[(Coordinate, Coordinate)] =
    for(c1 <- coordinates.indices;
        c2 <- coordinates.indices if c1 < c2) yield (coordinates(c1), coordinates(c2))

  def getGalaxyCoordinates(universe: Universe): Seq[Coordinate] =
    for(x <- universe.indices;
         y <- universe(x).indices if universe(x)(y) == '#') yield (x, y)

  def calculateCoordinateDistance(first: Coordinate, second: Coordinate): Int =
    val (firstX, firstY) = first
    val (secondX, secondY) = second

    (firstX - secondX).abs + (firstY - secondY).abs

  def addExtraRows(lines: List[String])(using expansionFactor: Int): List[String] =
    lines.foldLeft(List.empty[String])((acc, line) =>
      if(line.forall(_ == '.'))
        List.fill(expansionFactor)(line) ++ acc
      else line :: acc
    ).reverse

  def transposeLines(lines: List[String]): List[String] =
    lines
      .map(_.toList)
      .transpose
      .map(_.mkString(""))

  def addExtraColumns(lines: List[String])(using expansionFactor: Int): List[String] =
    transposeLines(addExtraRows(transposeLines(lines)))

  def parse(lines: List[String])(using expansionFactor: Int): Universe =
    addExtraColumns(addExtraRows(lines))
