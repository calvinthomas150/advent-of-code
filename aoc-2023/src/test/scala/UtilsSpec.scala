import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class UtilsSpec extends AnyFlatSpec with should.Matchers:
  "getInputLines" should "return expected data based on mode and daynumber" in:
    val input = Utils.getInputLines("test", 1)
    val expectedResult = "1abc2,pqr3stu8vwx,a1b2c3d4e5f,treb7uchet"
    input.mkString(",") should be(expectedResult)

