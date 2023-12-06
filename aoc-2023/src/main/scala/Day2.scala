import Day2.Colour.{BLUE, GREEN, RED}
import Part.*

object Day2:

  case class Game(id: Int, rounds: List[Round])
  type Round = List[Cube]
  case class Cube(colour: Colour, count: Int)
  enum Colour(val limit:Int):
    case BLUE extends Colour(12)
    case GREEN extends Colour(13)
    case RED extends Colour(14)

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
    val result = maxInGames.map(_ * _ * _).sum

    Utils.printResult(Part2, result.toString)
    result

  def maxCubesInGame(game: Game):(Int,Int,Int) =
    val allCubes = game.rounds.flatten

    val maxColourMap =
      allCubes
        .groupBy(_.colour)
        .view
        .mapValues(_.map(_.count).max)
      .toMap

    ( maxColourMap(RED),
      maxColourMap(GREEN),
      maxColourMap(BLUE))


  def isGameValid(game: Game): Boolean =
    game.rounds forall isRoundValid

  def isRoundValid(round:Round):Boolean =
    round.forall(c => isCubeValid(c))

  def isCubeValid(cube: Cube): Boolean =
    cube.count <= cube.colour.limit

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
    (roundInput split "," map createCube).toList

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




