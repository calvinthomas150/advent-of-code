

@main
def main(mode: String, dayNumber: Int): Unit =
  lazy val days =
    Map(1 -> (() => Day1.run(mode, dayNumber)),
        2 -> (() => Day2.run(mode, dayNumber)),
        3 -> (() => Day3.run(mode, dayNumber)),
        4 -> (() => Day4.run(mode, dayNumber)))

  days(dayNumber)()
