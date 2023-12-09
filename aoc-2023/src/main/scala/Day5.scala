import Part.*

object Day5:

  type Source = Long
  type Destination = Long

  enum Kind:
    case SEED
    case SOIL
    case FERTILIZER
    case WATER
    case LIGHT
    case TEMPERATURE
    case HUMIDITY
    case LOCATION

  case class Resource(start: Long, end: Long, kind: Kind)
  case class Property(destStart: Destination, srcStart: Source, srcEnd: Source ,rangeLength: Long)
  case class ResourceMap(from: Kind, to: Kind, properties: List[Property])

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def part1(lines: List[String]): Long =
    val seeds = getSeedsNoRange(lines)
    val maps = getResourceMap(lines)
    val result = calculate(seeds, maps)
    Utils.printResult(Part1, result.toString)
    result

  def part2(lines: List[String]): Long =
    val seeds = getSeedsRange(lines)
    val maps = getResourceMap(lines)
    val result = calculate(seeds, maps)
    Utils.printResult(Part2, result.toString)
    result

  def getSeedsData(lines: List[String]): List[Long] =
    lines
      .head
      .drop(7)
      .split(" ")
      .map(_.trim.toLong)
      .toList

  def getSeedsNoRange(lines: List[String]): List[Resource] =
    getSeedsData(lines)
      .map: start =>
        Resource(start = start, end = start, kind = Kind.SEED)

  def getSeedsRange(lines: List[String]): List[Resource] =
    getSeedsData(lines)
      .grouped(2)
      .map:
        case List(start, length) =>
          Resource(start = start, end = start + length - 1, kind = Kind.SEED)
      .toList

  def getResourceMap(lines: List[String]): List[ResourceMap] =

    val identifierMap =
      Map(
        "seed-to-soil map:" -> (Kind.SEED, Kind.SOIL),
        "soil-to-fertilizer map:" -> (Kind.SOIL, Kind.FERTILIZER),
        "fertilizer-to-water map:" -> (Kind.FERTILIZER, Kind.WATER),
        "water-to-light map:" -> (Kind.WATER, Kind.LIGHT),
        "light-to-temperature map:" -> (Kind.LIGHT, Kind.TEMPERATURE),
        "temperature-to-humidity map:" -> (Kind.TEMPERATURE, Kind.HUMIDITY),
        "humidity-to-location map:" -> (Kind.HUMIDITY, Kind.LOCATION))

    val identifiers: Seq[String] = List(
        "seed-to-soil map:",
        "soil-to-fertilizer map:",
        "fertilizer-to-water map:",
        "water-to-light map:",
        "light-to-temperature map:",
        "temperature-to-humidity map:",
        "humidity-to-location map:")

    def getProperties(lines:List[String], identifier: String): List[Property] =
      lines
      .dropWhile(!_.matches(identifier))
      .drop(1)
      .takeWhile(_.nonEmpty)
      .map: line =>
        val Array(dest, source, length) = line.split(" ").map(_.trim.toLong)
        Property(dest, source, source + length - 1 ,length)

    identifiers.map: identifier =>
      val (from:Kind, to:Kind) = identifierMap(identifier)
      ResourceMap(from, to, getProperties(lines, identifier).sortBy(_.srcStart))
    .toList

  def findNextResource(resource: Resource, resourceMap: ResourceMap): Seq[Resource] =

    def hasOverlaps(start: Long, end: Long, propertyStart: Long, propertyEnd: Long) =
      start >= propertyStart && start <= propertyEnd
      || end >= propertyStart && end <= propertyEnd
      || start <= propertyStart && end >= propertyEnd

    def getInsideRange(start: Long, end: Long, to: Kind ,prop: Property, overlaps: Boolean) =
      if (overlaps) then
        val delay = prop.destStart - prop.srcStart
        Some(Resource(
          Math.max(start, prop.srcStart) + delay,
          Math.min(end, prop.srcEnd) + delay,
          to
        ))
      else None

    def getUnderRange(start: Long, end:Long ,prop: Property, to: Kind): Option[Resource] =
      if(start < prop.srcStart)
        Option(Resource(start, Math.min(prop.srcStart - 1, end), to))
      else None

    def getOverRange(start: Long, end: Long, prop: Property, to: Kind) =
      if(end > prop.srcEnd) Some(Resource(Math.max(start, prop.srcEnd + 1), end, to))
      else None

    val (newResources, res) =
      val initial = (Nil:List[Resource], Option(resource))
      resourceMap.properties.foldLeft(initial) {
        case ((acc, Some(resource)), prop) =>
          val Resource(start, end, _) = resource

          val overlaps = hasOverlaps(start, end, prop.srcStart, prop.srcEnd)
          val inRange = getInsideRange(start, end, resourceMap.to, prop, overlaps)
          val underRange = getUnderRange(start, end, prop, resourceMap.to)
          val overRange = getOverRange(start, end, prop, resourceMap.to)
          (List(underRange, inRange, acc).flatten, overRange)

        case ((acc, None), _) => (acc, None)
      }
    List(newResources, res).flatten

  def calculate(seeds: List[Resource], resourceMaps: List[ResourceMap]): Long =
    def loop(resource: Resource): List[Resource] =
      if (resource.kind == Kind.LOCATION) List(resource)
      else
        val resourceMap = resourceMaps.find(_.from == resource.kind).get
        findNextResource(resource, resourceMap).flatMap(loop).toList

    seeds.flatMap(loop).minBy(_.start).start






