import Part.{Part1, Part2}

object Day3:
  
  def run(mode: String, dayNumber: Int): Unit =
    val data = Utils.getInput(mode, dayNumber)
    part1(data)
    part2(data)
  
  def part1(string: String): Unit =
    
    val re = "mul\\((\\d+),(\\d+)\\)".r
    val multiples = re.findAllIn(string)
    
    val result = multiples.map {
      case s"mul($x,$y)" => x.toInt * y.toInt
    }.sum
    
    Utils.printResult(Part1, result.toString)
      
    

  def part2(strings: String): Unit =
    val re = "(do\\(\\)|don't\\(\\)|mul\\((\\d+),(\\d+)\\))".r
    val parsed = re.findAllIn(strings)
    
    val result = parsed.foldLeft((0, true)){ case ((curr, enabled), elem) =>

      elem match
        case "do()" => (curr, true)
        case "don't()" => (curr, false)
        case s"mul($x,$y)" if enabled => (curr + (x.toInt * y.toInt), enabled)
        case _ => (curr, enabled)
    }
    
    Utils.printResult(Part2, result._1.toString)
    
    
    

