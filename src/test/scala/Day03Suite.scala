import Day03.*
import Day03Suite.*
import munit.ScalaCheckSuite

class Day03Suite extends ScalaCheckSuite:

  test("parsing \"mul(42,1)\""):
    assertEquals(Mul.parser.parse("mul(42,1)"), Right(("", Mul(42, 1))))

  test("parsing Muls only from small input"):
    assertEquals(
      mulsOnlyParser.parseAll(smallInput.head),
      Right(List(Mul(2, 4), Mul(5, 5), Mul(11, 8), Mul(8, 5)))
    )

  test("eval Muls only from small input returns 161"):
    assertEquals(evalMulsOnly(smallInput), Right(161))

  test("eval Muls only from big input returns 184_511_516"):
    assertEquals(evalMulsOnly(bigInput), Right(184_511_516))

object Day03Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day03_input.txt")

  val smallInput: List[String] = List("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
