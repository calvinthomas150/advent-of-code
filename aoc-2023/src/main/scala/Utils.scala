import scala.io.{BufferedSource, Source}

object Utils:
  def getInputLines(mode: String, dayNumber: Int): Iterator[String] =
    val runningMode = getMode(mode)
    Source.fromResource(s"${runningMode.filePath}/day$dayNumber.txt").getLines

  private def getMode(mode:String): Mode =
    mode match
      case "test" => Mode.TEST
      case "live" => Mode.LIVE
      case default => throw MatchError("Expected either test or live")


