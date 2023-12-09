import Day5.{Property, ResourceMap}
import Day5.Kind.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class Day5Spec extends AnyFlatSpec with should.Matchers:
  "getSeeds" should "return a list of numbers as the seeds" in:
    val lines:List[String] = List("seeds: 79 14 55 13", "", "seed - to - soil map:")
    Day5.getSeedsData(lines) should be(List(79,14,55,13))

  "getResources" should "return a list of the resource maps" in:
    val lines:List[String] = Utils.getInputLines("test", 5).toList
    val expected =
      List(ResourceMap(SEED, SOIL, List(Property(52, 50, 97, 48), Property(50, 98, 99, 2))),
        ResourceMap(SOIL, FERTILIZER, List(Property(39, 0, 14, 15), Property(0, 15, 51, 37), Property(37, 52, 53, 2))),
        ResourceMap(FERTILIZER, WATER, List(Property(42, 0, 6, 7), Property(57, 7, 10, 4), Property(0, 11, 52, 42), Property(49, 53, 60, 8))),
        ResourceMap(WATER, LIGHT, List(Property(88, 18, 24, 7), Property(18, 25, 94, 70))),
        ResourceMap(LIGHT, TEMPERATURE, List(Property(81, 45, 63, 19), Property(68, 64, 76, 13), Property(45, 77, 99, 23))),
        ResourceMap(TEMPERATURE, HUMIDITY, List(Property(1, 0, 68, 69), Property(0, 69, 69, 1))),
        ResourceMap(HUMIDITY, LOCATION, List(Property(60, 56, 92, 37), Property(56, 93, 96, 4))))

    Day5.getResourceMap(lines) should be(expected)

  "part1" should "return the correct value" in:
    val lines = Utils.getInputLines("test", 5).toList
    Day5.part1(lines) should be(35)

  "part2" should "return the correct value" in :
    val lines = Utils.getInputLines("test", 5).toList
    Day5.part2(lines) should be(46)
