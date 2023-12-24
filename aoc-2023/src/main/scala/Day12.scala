import Part.*

import scala.annotation.tailrec

object Day12:

  type Springa = List[Char]
  type DamagedGroups = Array[Int]

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def part1(lines: List[String]): Long =
    val parsed = parse(lines)
    val combinations: Seq[(Seq[Springa], DamagedGroups)] = 
      parsed.map((springs, damagedGroups) => (createPossibleSpringCombinations(springs), damagedGroups))
      
    val groups = 
      combinations
        .map((springList, damaged) => 
          (springList.map(countGroups)
            .map(_.filter(_ != 0)),damaged.toList)
        )
      
    val result = 
      groups.map(
        (springs, groups) => springs.filter(_ == groups))
        .map(_.size)
        .sum

    Utils.printResult(Part1, result.toString)
    result

  def part2(lines: List[String]): Long =
    ???
  
  def countGroups(springs: Springa): List[Int] =
    springs.foldLeft(List.empty[Int])((acc, char) =>
      acc match
        case Nil => if(char == '#') List(1) else List(0)
        case head :: tail =>
          if(char == '#') (head + 1) :: tail
          else
            0 :: acc).reverse

  def createPossibleSpringCombinations(springs: Springa): Seq[Springa] =
    springs.foldRight(List(List.empty[Char])): (char, acc) =>
      char match
        case '?' =>
          for(
            springs <- acc;
            char <- List('.', '#')) yield char :: springs
        case _ =>
          acc.map(comb => char :: comb)

  def parse(lines: List[String]): List[(Springa, DamagedGroups)] =
    lines
      .map(_.split(" "))
      .map:
        case Array(x, y) => (x.toList, y.split(",").map(_.toInt))