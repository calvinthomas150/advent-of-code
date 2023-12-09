import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day9Spec extends AnyFlatSpec with should.Matchers:
  val lines = Utils.getInputLines("test", 9).toList

  "parse" should "return the data parsed to a List[List[Int]]" in:
    Day9.parse(lines) should be(
      List(
        List(0, 3, 6, 9, 12, 15),
        List(1, 3, 6, 10, 15, 21),
        List(10, 13, 16, 21, 30, 45)
      )
    )

  "getDifferences" should "return the differences between each of the values from left to right" in:
    val history = Vector(1, 3, 6, 10, 15, 21)
    Day9.getDifferences(history) should be(Vector(2, 3, 4, 5, 6))

  "allDifferences" should "return a list of all the differences until we reach all zeros" in:
    val history = Vector(1, 3, 6, 10, 15, 21)
    val expected = Vector(Vector(0, 0, 0), Vector(1, 1, 1, 1),Vector(2, 3, 4, 5, 6))
    Day9.allDifferences(history) should be(expected)

  "part1" should "return the result of part 1" in:
    Day9.part1(lines) should be(114)

  "part2" should "return the result of part 2" in :
    Day9.part2(lines) should be(2)


