import Day5.getNextItemInChain
import Part.*

case class MapRange(destinationStart:Long, sourceStart: Long, length: Long)

object Day5:

  val identifiers: Seq[String] = List(
    "seed-to-soil map:",
    "soil-to-fertilizer map:",
    "fertilizer-to-water map:",
    "water-to-light map:",
    "light-to-temperature map:",
    "temperature-to-humidity map:",
    "humidity-to-location map:"
  )

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)

  def part1(lines: List[String]): Long =
    val seeds = getSeeds(lines)
    val transformations = identifiers.map(getMap(lines, _))
    val locations = transformations.foldLeft(seeds):
      (currentList, map) => currentList.map(item => getNextItemInChain(item, map))

    val result = locations.min
    Utils.printResult(Part1, result.toString)
    result

  def getNextItemInChain(input: Long, map: Seq[MapRange]): Long =
    map
      .find(s => s.sourceStart <= input && s.sourceStart + s.length >= input)
      .map(s => input - s.sourceStart + s.destinationStart)
      .getOrElse(input)

  def getSeeds(lines: List[String]): List[Long] =
    lines
      .head
      .drop(7)
      .split(" ")
      .map(_.trim.toLong)
      .toList

  def getMap(lines: List[String], identifier: String): Seq[MapRange] =
    val infoList = lines
      .dropWhile(!_.matches(identifier))
      .drop(1)
      .takeWhile(_.nonEmpty)
      .map(_.split(" "))
      .map(_.map(_.trim.toLong))

    infoList
      .map((item: Array[Long]) => MapRange(item.head, item.tail.head, item.last))

