

@main
def main(mode: String, dayNumber: Int): Unit =
  lazy val days =
    Map(1 -> (() => Day1.run(mode, dayNumber)),
        2 -> (() => Day2.run(mode, dayNumber)),
        3 -> (() => Day3.run(mode, dayNumber)),
        4 -> (() => Day4.run(mode, dayNumber)),
        5 -> (() => Day5.run(mode, dayNumber)),
        6 -> (() => Day6.run(mode, dayNumber)),
        7 -> (() => Day7.run(mode, dayNumber)),
        8 -> (() => Day8.run(mode, dayNumber)),
        9 -> (() => Day9.run(mode, dayNumber)),
        10 -> (() => Day10.run(mode, dayNumber)))

  days(dayNumber)()
