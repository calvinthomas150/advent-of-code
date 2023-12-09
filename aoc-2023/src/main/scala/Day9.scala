import Day9.VectorPosition.*
import Part.*

import scala.annotation.tailrec

object Day9:

  type History = Vector[Int]
  type Histories = Vector[History]
  type Differences = Vector[Int]

  enum VectorPosition:
    case FRONT
    case BACK

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def part1(lines: List[String]): Int =
    val histories = parse(lines)
    val result =
      (histories map:
        history =>
          val diffsWithHistory: Vector[Differences] = allDifferences(history) :+ history
          updateAllDifferences(diffsWithHistory)(BACK).last.last).sum

    Utils.printResult(Part1, result.toString)
    result

  def part2(lines: List[String]): Int =
    val histories = parse(lines)
    val result = (histories map :
      history =>
        val diffsWithHistory: Vector[Differences] = allDifferences(history) :+ history
        updateAllDifferences(diffsWithHistory)(FRONT).last.head).sum

    Utils.printResult(Part2, result.toString)
    result

  def updateAllDifferences(differences: Vector[Differences])(implicit position: VectorPosition): Vector[Differences] =
    @tailrec
    def loop(diffs: Vector[Differences], updatedDiffs:Vector[Differences]): Vector[Differences] =
      if(diffs.size == 1) updatedDiffs
      else loop(
        updateDifference(diffs.head, diffs.tail.head) +: diffs.drop(2), updatedDiffs :+
          updateDifference(diffs.head, diffs.tail.head))

    loop(differences, Vector.empty)

  def updateDifference(previous: Differences, current: Differences)(implicit position:VectorPosition): Differences =
    position match
      case FRONT => (current.head - previous.head) +: current
      case BACK => current :+ previous.last + current.last

  def allDifferences(history: History): Vector[Differences] =
    @tailrec
    def loop(history: History | Differences, allDifferences: Vector[Differences]): Vector[Differences] =
      val differences = getDifferences(history)
      if(differences.forall(_ == 0)) differences +: allDifferences
      else loop(differences, differences +: allDifferences)

    loop(history, Vector.empty)

  def getDifferences(history: History): Differences =
    for(h <- history.sliding(2).toVector) yield h.last - h.head

  def parse(lines: List[String]): Histories =
    (for line <- lines
      yield line.split(" ").map(_.toInt).toVector).toVector