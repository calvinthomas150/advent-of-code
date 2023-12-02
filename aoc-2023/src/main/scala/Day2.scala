import Day2.Colour.{BLUE, GREEN}
import Part.*

object Day2:

  def run(mode:String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    val games = setup(lines)
    part1(games)
    part2(games)

  def part1(games: List[Game]): Int =
    val validGames = games filter isGameValid
    val result = validGames.map(_.id).sum
    Utils.printResult(Part1, result.toString)
    result

  def part2(games: List[Game]): Int =
    val maxInGames = games map maxCubesInGame
    val result = maxInGames.map:
      case (Cube(_, redCount), Cube(_, greenCount), Cube(_, blueCount)) =>
      redCount * greenCount * blueCount
    .sum

    Utils.printResult(Part2, result.toString)
    result

  def maxCubesInGame(game: Game):(Cube,Cube,Cube) =
    val allCubes = game.rounds.flatMap(r => r.cubes)
    val maxBlue = allCubes.filter(_.colour == Colour.BLUE).maxBy(_.count)
    val maxGreen = allCubes.filter(_.colour == Colour.GREEN).maxBy(_.count)
    val maxRed = allCubes.filter(_.colour == Colour.RED).maxBy(_.count)
    (maxRed, maxGreen, maxBlue)

  def isGameValid(game: Game): Boolean =
    game.rounds.forall(r => isRoundValid(r))

  def isRoundValid(round:Round):Boolean =
    round.cubes.forall(c => isCubeValid(c))

  def isCubeValid(cube: Cube): Boolean =
    val maxCubes: Map[Colour, Int] =
      Map(Colour.RED -> 12,
        Colour.GREEN -> 13,
        Colour.BLUE -> 14)

    maxCubes(cube.colour) >= cube.count

  def setup(input: List[String]): List[Game] =
    input map createGame

  def createGame(gameInput: String): Game =
    val gameId = createGameId(gameInput)
    val rounds = createRounds(gameInput)
    Game(gameId, rounds)

  def createGameId(gameInput: String): Int =
    gameInput.drop(5).takeWhile(_ != ':').toInt

  def createRounds(gameInput: String): List[Round] =
    val roundsInput = createRoundsInput(gameInput)
    roundsInput map createRound

  def createRound(roundInput: String): Round =
    Round(roundInput split "," map createCube)

  def createCube(cubeInput: String): Cube =
    val count = cubeInput.trim.takeWhile(_ != ' ').toInt
    val colour = Colour.valueOf(cubeInput.trim.dropWhile(_ != ' ').trim.toUpperCase)
    Cube(colour = colour, count = count)

  def createRoundsInput(gameInput:String): List[String] =
    gameInput
      .dropWhile(_ != ':')
      .drop(1)
      .split(";")
      .toList

  case class Game(id: Int, rounds: List[Round])

  case class Round(cubes: Array[Cube]):
    override def equals(obj: Any): Boolean = obj match
      case Round(otherCubes) => cubes.sameElements(otherCubes)
      case _ => false


  case class Cube(colour: Colour, count: Int)

  enum Colour:
    case BLUE
    case GREEN
    case RED

