import Part.Part1

import scala.annotation.tailrec

object Day8:

  type Node = String
  type NodeMap = Map[Node, (Node, Node)]

  enum Direction(val string: Char):
    case LEFT extends Direction('L')
    case RIGHT extends Direction('R')

  object Direction:
    def from(char: Char): Direction =
      char match
        case 'L' => Direction.LEFT
        case 'R' => Direction.RIGHT

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)


  def part1(lines: List[String]): Int =
    val (directions, nodeMap) = parse(lines)
    val startNode = "AAA"

    @tailrec
    def countSteps(dirs: List[Direction], node: Node, steps: Int): Int =
      if(node == "ZZZ") steps
      else
        val nextNode = getNextNode(dirs.head, node, nodeMap)
        if(dirs.size == 1) countSteps(directions, nextNode, steps + 1)
        else countSteps(dirs.tail, nextNode, steps + 1)

    val result = countSteps(directions, startNode, 0)
    Utils.printResult(Part1, result.toString)
    result


  def part2(lines: List[String]): Int =
    ???

  def getNextNode(direction: Direction, currentNode:Node, nodeMap: NodeMap): Node =
    direction match
      case Direction.LEFT => nodeMap(currentNode)._1
      case Direction.RIGHT => nodeMap(currentNode)._2

  def parse(lines: List[String]): (List[Direction], NodeMap) =
    (parseDirections(lines.head), parseNodes(lines))

  def parseNodes(lines: List[String]): NodeMap =
    (lines.drop(2).map:
      case s"$node = ($left, $right)" => (node, (left, right))).toMap

  def parseDirections(lines:String): List[Direction] =
    lines.map(Direction.from).toList




