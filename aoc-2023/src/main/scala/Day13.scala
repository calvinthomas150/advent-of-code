import Part.*

import scala.io.Source

object Day13:

  type Pattern = List[List[Char]]

  def run(mode: String, dayNumber: Int): Unit =
    val input = getInput(mode, dayNumber)
    val patterns = getPatterns(input)
    part1(patterns)
    part2(patterns)

  def part1(patterns: List[Pattern]): Int =
    val horizontal = patterns.flatMap(findReflectionPoint)
    val vertical = patterns.map(_.transpose).flatMap(findReflectionPoint)
    val result = horizontal.map(_ * 100).sum + vertical.sum

    Utils.printResult(Part1, result.toString)
    result

  def part2(patterns: List[Pattern]): Int =
    val horizontal = patterns.flatMap(findSmudge)
    val vertical = patterns.map(_.transpose).flatMap(findSmudge)
    val result = horizontal.map(_ * 100).sum + vertical.sum

    Utils.printResult(Part2, result.toString)
    result

  def findReflectionPoint(pattern: Pattern):Option[Int] =
    (1 until pattern.size).find: i =>
      val (left, right) = pattern.splitAt(i)
      left.reverse.zip(right).forall(_ == _)

  def findSmudge(pattern: Pattern): Option[Int] =
    (1 until pattern.size).find: i =>
      val (left, right) = pattern.splitAt(i)
      left
        .reverse
        .zip(right)
        .map((l1, l2) => l1.zip(l2).count(_ != _))
        .sum == 1


  def getInput(mode: String, dayNumber: Int): String =
    val runningMode = Utils.getMode(mode)
    Source.fromResource(s"${runningMode.filePath}/day$dayNumber.txt").mkString("")

  def getPatterns(input: String): List[Pattern] =
    input
      .split("\n\n")
      .toList
      .map(_.split("\n")
      .toList
      .map(_.toList))

