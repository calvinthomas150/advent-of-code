import Part.*

object Day2:

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def parse(lines: List[String]): List[List[Int]] =
    lines.map(_.split("\\s+").map(_.toInt).toList)
    
  def part1(strings: List[String]): Unit =
    val parsed = parse(strings)
    Utils.printResult(Part1, parsed.map(isSafe).count(identity).toString)
      
  def part2(strings: List[String]): Unit =
    val parsed = parse(strings)
    Utils.printResult(Part2, parsed.map(isSafeWithDampener).count(identity).toString)

  def isSafeWithDampener(ints: List[Int]): Boolean = {
    ints.indices.exists { i =>
      isSafe(ints.take(i) ++ ints.drop(i + 1))
    }
  }
  
  def isSafe(ints: List[Int]): Boolean =
    val zipped = ints.zip(ints.tail)
    zipped.forall((f, s) => s > f && s <= f + 3) ||
    zipped.forall((f, s) => f > s && f <= s + 3)
