import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Day1Spec extends AnyFlatSpec with should.Matchers:

 "Removing characters" should "only leave the numeric values" in:
   val input = List("1 abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet").iterator
   val removed = Day1.removeAllNonNumericCharacters(input).toList
   val expected = List("12","38","12345","7")
   removed should be(expected)


 "First and last" should "only return the first and last numbers from each string" in:
   val input = List("12","38","12345","7").iterator
   val expected = List((1,2), (3,8), (1,5), (7,7))
   Day1.firstAndLastNumbers(input).toList should be(expected)


 "Sum of first and last" should "sum each tuple and then sum the result" in:
   val input = List((1,2), (3,8), (1,5), (7,7)).iterator
   val expected = 12 + 38 + 15 + 77
   Day1.combineAndSumFirstAndLast(input) should be (expected)


 "Removing characters with numeric words" should "leave numeric characters and word versions of them " in:
   val input = List("1 two ab", "3 for", "5").iterator
   val removed = Day1.removeAllNonNumericWithWordsCharacters(input).toList
   val expected = List("12","3", "5")
   removed should be(expected)
