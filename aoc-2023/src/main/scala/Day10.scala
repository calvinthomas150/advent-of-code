import Day10.Pipe.*
import Part.Part1
import scala.annotation.tailrec


object Day10:

  type Coordinate = (Int, Int)

  case class Grid(items: Vector[Vector[Pipe]]):
    override def toString: String =
      val symbols: Vector[String] =
        for (pipes <- items) yield pipes.map(_.symbol).mkString
      symbols.mkString("\n")

  enum Pipe(val symbol: Char):
    case UPDOWN extends Pipe('|')
    case LEFTRIGHT extends Pipe('-')
    case UPRIGHT extends Pipe('L')
    case UPLEFT extends Pipe('J')
    case DOWNLEFT extends Pipe('7')
    case DOWNRIGHT extends Pipe('F')
    case NONE extends Pipe('.')
    case START extends Pipe('S')

  object Pipe:
    def from(char: Char): Pipe =
      char match
        case '|' => UPDOWN
        case '-' => LEFTRIGHT
        case 'L' => UPRIGHT
        case 'J' => UPLEFT
        case '7' => DOWNLEFT
        case 'F' => DOWNRIGHT
        case '.' => NONE
        case 'S' => START

  def findStartPosition(grid: Grid): Coordinate =
    (for(x <- grid.items.indices;
         y <- grid.items(x) if y == START) yield (x, grid.items(x).indexOf(START)))
    .head

  def getNeighbourCoordinates(grid: Grid, coordinate: Coordinate): Vector[Coordinate] =
    val (x, y) = coordinate

    val up = (x - 1, y)
    val down = (x + 1, y)
    val left = (x, y - 1)
    val right = (x, y + 1)

    val coordinates =
      Vector(up, down, left, right)
        .filter((x, y) =>
          x >= 0
          && grid.items.size - 1 >= x
          && y >= 0
          && grid.items.head.size -1 >= y)

    coordinates

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def part1(lines: List[String]): Int =
    val grid = parse(lines)
    val start = findStartPosition(grid)
    val stack = List((start, 0))
    val visited = Set.empty[Coordinate]

    @tailrec
    def search(toVisit: List[(Coordinate, Int)], visited: Set[Coordinate], max: Int): Int =
      toVisit match
        case Nil => max / 2 + 1
        case (coordinate, steps) :: remaining =>
          val newMax = math.max(max, steps)
          val neighbours = getNeighbourCoordinates(grid, coordinate)
            .filterNot(neighbour =>
              grid.items(neighbour._1)(neighbour._2) == NONE || toVisit.exists(_._1 == neighbour))
          val validNeighbours =
            neighbours.filter(neighbour =>
              val (x, y) = neighbour
              val (coX, coY) = coordinate
              val dx = x - coX
              val dy = y - coY
              !visited.contains(neighbour) && isConnecting(grid.items(coX)(coY), grid.items(x)(y), dx, dy))
          val newToVisit = validNeighbours.map(_ -> (steps + 1)) ++ remaining
          val newVisited = visited ++ validNeighbours
          search(newToVisit.toList, newVisited, newMax)

    val result = search(stack, visited, 0)
    Utils.printResult(Part1, result.toString)
    result

  def isConnecting(current: Pipe, neighbour: Pipe, dx: Int, dy: Int): Boolean =
    val up = dx == -1 && dy == 0
    val down = dx == 1 && dy == 0
    val left = dx == 0 && dy == -1
    val right = dx == 0 && dy == 1

    def startValid(neighbour: Pipe) =
      (List(UPDOWN, DOWNLEFT, DOWNRIGHT).contains(neighbour) && up)
      || (List(UPDOWN, UPRIGHT, UPLEFT).contains(neighbour) && down)
      || (List(LEFTRIGHT, DOWNRIGHT, UPRIGHT).contains(neighbour) && left)
      || (List(LEFTRIGHT, UPLEFT, DOWNLEFT).contains(neighbour) && right)

    def upDownValid(neighbour: Pipe) =
      (List(DOWNRIGHT, DOWNLEFT).contains(neighbour) && up)
      || (List(UPRIGHT, UPLEFT).contains(neighbour) && down)
      || (UPDOWN == neighbour && dy == 0)

    def leftRightValid(neighbour:Pipe) =
      (List(DOWNRIGHT, UPRIGHT).contains(neighbour) && left)
      || (List(UPLEFT, DOWNLEFT).contains(neighbour) && right)
      || (LEFTRIGHT == neighbour && dx == 0)

    def upLeftValid(neighbour: Pipe) =
      (List(DOWNLEFT, DOWNRIGHT, UPDOWN).contains(neighbour) && up)
      || (List(UPRIGHT, DOWNRIGHT, LEFTRIGHT).contains(neighbour) && left)

    def upRightValid(neighbour: Pipe) =
      (List(DOWNLEFT, DOWNRIGHT, UPDOWN).contains(neighbour) && up) ||
      (List(UPLEFT, LEFTRIGHT, DOWNLEFT).contains(neighbour) && right)

    def downRightValid(neighbour: Pipe) =
      (List(UPLEFT, UPDOWN, UPRIGHT).contains(neighbour) && down)
      || (List(DOWNLEFT, UPLEFT, LEFTRIGHT).contains(neighbour) && right)

    def downLeftValid(neighbour: Pipe) =
      (List(DOWNRIGHT, UPRIGHT, LEFTRIGHT).contains(neighbour) && left)
      || (List(UPLEFT, UPDOWN, UPRIGHT).contains(neighbour) && down)

    current match
      case START => startValid(neighbour)
      case UPDOWN => upDownValid(neighbour)
      case LEFTRIGHT => leftRightValid(neighbour)
      case UPLEFT => upLeftValid(neighbour)
      case UPRIGHT => upRightValid(neighbour)
      case DOWNRIGHT => downRightValid(neighbour)
      case DOWNLEFT => downLeftValid(neighbour)
      case NONE => false

  def part2(lines: List[String]): Int =
    ???

  def parse(lines: List[String]): Grid =
    Grid(lines
      .map(_.map(Pipe.from).toVector)
      .toVector)
