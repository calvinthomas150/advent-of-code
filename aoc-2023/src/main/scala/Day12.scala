import Part.*

import scala.collection.mutable

object Day12:

  type Springs = List[Char]
  type DamagedCounts = List[Int]

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def countLine(line: String): Long =
    val Array(s, dc) = line.split(" ")
    val springs: Springs = s.toList
    val damagedCounts: DamagedCounts = dc.split(",").map(_.toInt).toList
    count(springs, damagedCounts)

  val cache = mutable.Map.empty[(Springs, DamagedCounts, Int), Long]

  def count(springs: Springs, damagedCounts: DamagedCounts, damagedCount: Int = 0): Long =
    cache.getOrElseUpdate((springs, damagedCounts, damagedCount), countCombinations(springs, damagedCounts, damagedCount))

  def countCombinations(springs: Springs, damagedCounts: DamagedCounts, damagedCount: Int = 0): Long =
    if(springs.isEmpty)
      if(damagedCounts.isEmpty && damagedCount == 0) 1L
      else if(damagedCounts.length == 1 && damagedCounts.head == damagedCount) 1L
      else 0L
    else
      def working() =
        if(damagedCount == 0) count(springs.tail, damagedCounts)
        else if(damagedCounts.nonEmpty && damagedCounts.head == damagedCount) count(springs.tail, damagedCounts.tail)
        else 0L
      def damaged() =
        if(damagedCounts.isEmpty) 0L
        else count(springs.tail, damagedCounts, damagedCount + 1)
      springs.head match
        case '?' => working() + damaged()
        case '.' => working()
        case '#' => damaged()

  def part1(lines: List[String]): Long =
    val result = lines.map(countLine).sum
    Utils.printResult(Part1, result.toString)
    result

  def part2(lines: List[String]): Long =
    val springs = for(line <- lines) yield (line.takeWhile(_ != ' ') + '?') * 4 ++ line.takeWhile(_ != ' ')
    val damagedCounts =
      for(line <- lines)
        yield ((line.dropWhile(_ != ' ').drop(1) + ',') * 4) ++ line.dropWhile(_ != ' ').drop(1)

    val updatedLines =
      springs
        .zip(damagedCounts)
        .map(s => s"${s._1} ${s._2}")

    val result = updatedLines.map(countLine).sum
    Utils.printResult(Part2, result.toString)
    result

