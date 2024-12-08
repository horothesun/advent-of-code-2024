import Day03.*
import Day03.Op.*
import Day03.SeparatedElement.*
import Day03Suite.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class Day03Suite extends ScalaCheckSuite:

  // part 1

  test("parsing \"mul(42,1)\""):
    assertEquals(Mul.parser.parse("mul(42,1)"), Right(("", Mul(42, 1))))

  test("parsing Muls only from small input 1"):
    assertEquals(
      mulsOnlyParser.parseAll(smallInput1.head),
      Right(List(Mul(2, 4), Mul(5, 5), Mul(11, 8), Mul(8, 5)))
    )

  test("eval Muls only from small input 1 returns 161"):
    assertEquals(evalMulsOnly(smallInput1), Right(161))

  test("eval Muls only from big input returns 184_511_516"):
    assertEquals(evalMulsOnly(bigInput), Right(184_511_516))

  // part 2

  test("split by Sep sample"):
    def elem(i: Int): Elem[Int, String | Char] = Elem(i)
    def sep(s: String | Char): Sep[Int, String | Char] = Sep(s)
    val l = List[SeparatedElement[Int, String | Char]](
      elem(42),
      sep("hi"),
      elem(1),
      sep('x'),
      elem(2),
      elem(3),
      sep("bye"),
      elem(4)
    )
    assertEquals(
      l.splitBySep(start = "START"),
      List[(Sep[Int, String | Char], List[Elem[Int, String | Char]])](
        (sep("START"), List(elem(42))),
        (sep("hi"), List(elem(1))),
        (sep('x'), List(elem(2), elem(3))),
        (sep("bye"), List(elem(4)))
      )
    )

  property("split by Sep preserves length"):
    forAll(
      Gen.listOf(
        Gen.oneOf[SeparatedElement[Int, String | Char]](
          Gen.posNum[Int].map(Elem[Int, String | Char].apply),
          Gen
            .oneOf[String | Char](Gen.stringOfN(3, Gen.alphaChar), Gen.alphaChar)
            .map(Sep[Int, String | Char].apply)
        )
      )
    ) { l =>
      assertEquals(
        l.splitBySep(start = "START").map(1 + _._2.length).sum,
        List("START").length + l.length
      )
    }

  test("group Ops sample"):
    assertEquals(
      group(
        List[Op](
          Multiply(2, 4),
          DoNot,
          Multiply(5, 5),
          Multiply(11, 8),
          Do,
          Multiply(8, 5)
        )
      ),
      List(
        (Do, List(Multiply(2, 4))),
        (DoNot, List(Multiply(5, 5), Multiply(11, 8))),
        (Do, List(Multiply(8, 5)))
      )
    )

  test("eval enabled multiplications only from Ops sample"):
    assertEquals(
      evalEnabledMulsOnlyFromOps(
        List[Op](
          Multiply(2, 4),
          DoNot,
          Multiply(5, 5),
          Multiply(11, 8),
          Do,
          Multiply(8, 5)
        )
      ),
      expected = 48
    )

  test("parsing Ops from small input 2"):
    assertEquals(
      opsParser.parseAll(smallInput2.head),
      Right(
        List(
          Multiply(2, 4),
          DoNot,
          Multiply(5, 5),
          Multiply(11, 8),
          Do,
          Multiply(8, 5)
        )
      )
    )

  test("eval enabled multiplications only from small input 2 returns 48"):
    assertEquals(evalEnabledMulsOnly(smallInput2), Right(48))

  test("eval enabled multiplications only from big input returns 90_044_227"):
    assertEquals(evalEnabledMulsOnly(bigInput), Right(90_044_227))

object Day03Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day03_input.txt")

  val smallInput1: List[String] = List("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
  val smallInput2: List[String] = List("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
