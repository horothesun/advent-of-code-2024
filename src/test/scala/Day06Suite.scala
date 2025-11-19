import Day06.*
import Day06Suite.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class Day06Suite extends ScalaCheckSuite:

  test("42 == 42"):
    assertEquals(42, 42)

object Day06Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day06_input.txt")
