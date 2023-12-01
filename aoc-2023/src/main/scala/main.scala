

@main
def main(mode: String, dayNumber: Int): Unit =
  lazy val days: Map[Int, Unit] =
    Map(1 -> Day1.run(mode, dayNumber))

  days(dayNumber)
