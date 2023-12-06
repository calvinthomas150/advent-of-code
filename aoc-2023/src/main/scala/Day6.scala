import Part.*

object Day6:

  case class Race(time: Long, record: Long)

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def part1(lines: List[String]): Int =
    val races = getRacesPart1(lines)
    val combinations =
      for(race <- races;
        time <- BigInt(1) to BigInt(race.time)) yield (race, distanceTravelled(race, time.toLong), race.record)

    val result =
      combinations
        .groupBy((race, _,_) => race)
        .view
        .mapValues(_.filter((_,distance, record) => distance > record))
        .mapValues(_.size)
        .values
        .product

    Utils.printResult(Part1, result.toString)
    result

  def part2(lines:List[String]): Int =
    val race = getRacesPart2(lines)
    val combinations = for(time <- BigInt(1) to BigInt(race.time))
      yield (distanceTravelled(race, time.toLong), race.record)
    val result = combinations.count((distance, record) => distance > record)

    Utils.printResult(Part2, result.toString)
    result


  def distanceTravelled(race: Race, timeHeld:Long): Long =
    timeHeld * (race.time - timeHeld)

  def getRacesPart1(lines: List[String]): Seq[Race] =
    val List(t, d) = lines
    val times = parseLinePart1(t)
    val distance = parseLinePart1(d)
    times.zip(distance).map((t,d) => Race(t,d))

  def parseLinePart1(line: String): Seq[Int] =
    line
      .dropWhile(!_.isDigit)
      .split("\\s+")
      .map(_.toInt)
      .toList

  def getRacesPart2(lines: List[String]): Race =
    val List(t, d) = lines
    val time = parseLinePart2(t)
    val distance = parseLinePart2(d)
    Race(time, distance)

  def parseLinePart2(line:String): Long =
    line
      .dropWhile(!_.isDigit)
      .replaceAll("[^0-9]", "")
      .toLong