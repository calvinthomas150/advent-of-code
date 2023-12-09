import Part.Part1

import scala.annotation.tailrec

object Day9:

  type History = Vector[Int]
  type Histories = Vector[History]
  type Differences = Vector[Int]

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def part1(lines: List[String]) =
    val histories = parse(lines)
    val result = (histories map:
      history =>
        val all: Vector[Differences] = (allDifferences(history) :+ history)
        val updatedDiffs = updateAllDifferences(all)
        updateAllDifferences(all).last.last).sum

    Utils.printResult(Part1, result.toString)


  def updateAllDifferences(differences: Vector[Differences]) =
    def loop(diffs: Vector[Differences], updatedDiffs:Vector[Differences]): Vector[Differences] =
      if(diffs.size == 2)
        updatedDiffs :+ updateDifference(diffs.head, diffs.last)
      else
        loop(diffs.drop(2) :+ updateDifference(diffs.head, diffs.tail.head), updatedDiffs :+ updateDifference(diffs.head, diffs.tail.head))

    loop(differences, Vector.empty)

  def updateDifference(previous: Differences, current: Differences): Differences =
    current :+ previous.last + current.last

  def part2(lines: List[String]) =
    ???

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