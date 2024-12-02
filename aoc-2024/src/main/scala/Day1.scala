import Part.*

object Day1:

  def run(mode:String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def part1(lines: List[String]): Unit =
    val parsed = parse(lines)
    val (first, second) = (parsed.head, parsed.tail.head)
    val result = 
      first
        .sorted
        .zip(second.sorted)
        .map((f, s) => Math.abs(f - s))
        .sum
    Utils.printResult(Part1, result.toString)

  def part2(lines: List[String]): Unit =
    val parsed = parse(lines)
    val (first, second) = (parsed.head, parsed.tail.head)
    val result = first.foldLeft(Nil){ (acc:List[Int], elem:Int) =>
      val count = second.count(_ == elem)
      (elem * count) :: acc
    }.sum

    Utils.printResult(Part2, result.toString)
    
  def parse(lines: List[String]) =
    lines.map { line =>
      line
        .split("\\s+")
    }.map(_.toList)
      .transpose
      .map(_.map(_.toInt))


    
    

