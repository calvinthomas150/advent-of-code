import Day2.Colour.{BLUE, GREEN, RED}
import Day2.{Colour, Cube, Game, Round}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day2Spec extends AnyFlatSpec with should.Matchers:
  "createGameId" should "return the id of the game based on it being stored after 'Game ' and before : in the input" in:
    val gameInput1 = "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    Day2.createGameId(gameInput1) should be(5)
    val gameInput2 = "Game 51: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    Day2.createGameId(gameInput2) should be(51)

  "createRoundsInput" should "return a list of the rounds, separating out on the ; and without the starter 'Game XXX: ' text" in :
    val gameInput1 = "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    Day2.createRoundsInput(gameInput1) should be(List(" 6 red, 1 blue, 3 green"," 2 blue, 1 red, 2 green"))

  "createCube" should "return a cube with correct enum colour and count" in :
    val cubeInput = " 6 red"
    val expected = Cube(RED, 6)
    Day2.createCube(cubeInput) should be(expected)

  "createRound" should "return an Array of cubes objects" in:
    val roundInput = " 6 red, 1 blue, 3 green"
    val expected = Round(Array(Cube(RED, 6), Cube(BLUE, 1), Cube(GREEN, 3)))
    Day2.createRound(roundInput) should be(expected)

  "createRounds" should "return a List of Rounds" in:
    val gameInput1 = "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    val expected =
      List(Round(Array(Cube(RED, 6), Cube(BLUE, 1), Cube(GREEN, 3))),
        Round(Array(Cube(BLUE, 2), Cube(RED, 1), Cube(GREEN, 2)))
      )

  "createGame" should "return a Game consisting of the ID and a List of rounds" in:
    val gameInput1 = "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    val expectedGameId = 5
    val expectedRounds =
      List(Round(Array(Cube(RED, 6), Cube(BLUE, 1), Cube(GREEN, 3))),
        Round(Array(Cube(BLUE, 2), Cube(RED, 1), Cube(GREEN, 2)))
      )

    Day2.createGame(gameInput1) should be(Game(expectedGameId, expectedRounds))

  "iscubeValid" should "return true if the count is less than the max amount in the map" in:
    val cube = Cube(Colour.BLUE, 10)
    Day2.isCubeValid(cube) should be(true)

  "iscubeValid" should "return false if the count is greater than the max amount in the map" in :
    val cube = Cube(Colour.BLUE, 17)
    Day2.isCubeValid(cube) should be(false)

  "isRoundValid" should "return true if the count is less than the max amount in the map for all cubes in the round" in :
    val round = Round(Array(Cube(RED, 6), Cube(BLUE, 1), Cube(GREEN, 3)))
    Day2.isRoundValid(round) should be(true)

  "isRoundValid" should "return false if the count is greater than the max amount in the map for any cubes in the round" in :
    val round = Round(Array(Cube(RED, 6), Cube(BLUE, 1), Cube(GREEN, 30)))
    Day2.isRoundValid(round) should be(false)

  "isGameValid" should "return true if any of the rounds in the game are not valid" in:
    val gameId = 5
    val rounds =
      List(Round(Array(Cube(RED, 6), Cube(BLUE, 1), Cube(GREEN, 3))),
        Round(Array(Cube(BLUE, 2), Cube(RED, 1), Cube(GREEN, 2)))
      )
    val game = Game(gameId, rounds)

    Day2.isGameValid(game) should be(true)


  "isGameValid" should "return false if any of the rounds in the game are not valid" in :
    val gameId = 5
    val rounds =
      List(Round(Array(Cube(RED, 60), Cube(BLUE, 1), Cube(GREEN, 3))),
        Round(Array(Cube(BLUE, 2), Cube(RED, 1), Cube(GREEN, 2)))
      )
    val game = Game(gameId, rounds)

    Day2.isGameValid(game) should be(false)

  "part1" should "return the sum of the ids of all games when al games are valid" in :
    val gameId1 = 5
    val rounds1 =
      List(Round(Array(Cube(RED, 6), Cube(BLUE, 1), Cube(GREEN, 3))),
        Round(Array(Cube(BLUE, 2), Cube(RED, 1), Cube(GREEN, 2)))
      )
    val game1 = Game(gameId1, rounds1)

    val gameId2 = 6
    val rounds2 =
      List(Round(Array(Cube(RED, 6), Cube(BLUE, 1), Cube(GREEN, 3))),
        Round(Array(Cube(BLUE, 2), Cube(RED, 1), Cube(GREEN, 2)))
      )
    val game2 = Game(gameId2, rounds2)

    val games = List(game1,game2)

    Day2.part1(games) should be(11)

  "part1" should "return the sum of the ids of only valud games when some are invalid" in :
    val gameId1 = 5
    val rounds1 =
      List(Round(Array(Cube(RED, 60), Cube(BLUE, 1), Cube(GREEN, 3))),
        Round(Array(Cube(BLUE, 2), Cube(RED, 1), Cube(GREEN, 2)))
      )
    val game1 = Game(gameId1, rounds1)

    val gameId2 = 6
    val rounds2 =
      List(Round(Array(Cube(RED, 6), Cube(BLUE, 1), Cube(GREEN, 3))),
        Round(Array(Cube(BLUE, 2), Cube(RED, 1), Cube(GREEN, 2)))
      )
    val game2 = Game(gameId2, rounds2)

    val games = List(game1, game2)

    Day2.part1(games) should be(6)

  "maxCubesInGame" should "return the maximum number of cubes from any of the rounds in each game for each cube" in:
    val gameId = 5
    val rounds =
      List(Round(Array(Cube(RED, 6), Cube(BLUE, 1), Cube(GREEN, 3))),
        Round(Array(Cube(BLUE, 2), Cube(RED, 1), Cube(GREEN, 2)))
      )
    val game = Game(gameId, rounds)

    Day2.maxCubesInGame(game) should be((6,3,2))




