import Day4.{Scratchcard, part1}
import Day5.MapRange
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day5Spec extends AnyFlatSpec with should.Matchers:
  "getSeeds" should "return a list of numbers as the seeds" in:
    val lines:List[String] = List("seeds: 79 14 55 13", "", "seed - to - soil map:")
    Day5.getSeeds(lines) should be(List(79,14,55,13))

  "getMap" should "return a list of MapRange objects for the particular map" in:
    val lines:List[String] = Utils.getInputLines("test", 5).toList
    Day5.getMap(lines, "seed-to-soil map:") should be(List(MapRange(50, 98, 2), MapRange(52, 50, 48)))
    Day5.getMap(lines, "soil-to-fertilizer map:") should be(List(MapRange(0, 15, 37), MapRange(37, 52, 2), MapRange(39, 0, 15)))

  "part1" should "return the correct value" in:
    val lines = Utils.getInputLines("test", 5).toList
    Day5.part1(lines) should be(35)
