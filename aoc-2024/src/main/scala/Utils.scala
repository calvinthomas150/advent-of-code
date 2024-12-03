import scala.io.{BufferedSource, Source}

object Utils:
  def getInputLines(mode: String, dayNumber: Int): Iterator[String] =
    val runningMode = getMode(mode)
    Source.fromResource(s"${runningMode.filePath}/day$dayNumber.txt").getLines

  def getInput(mode: String, dayNumber: Int): String =
    val runningMode = Utils.getMode(mode)
    Source.fromResource(s"${runningMode.filePath}/day$dayNumber.txt").mkString("")

  def getMode(mode:String): Mode =
    mode match
      case "test" => Mode.TEST
      case "live" => Mode.LIVE
      case default => throw MatchError("Expected either test or live")

  def printResult(part: Part, result:String): Unit =
    part match
      case Part.Part1 => println(s"Part 1 Result: $result")
      case Part.Part2 => println(s"Part 2 Result: $result")


