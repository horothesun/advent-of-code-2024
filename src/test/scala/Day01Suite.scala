import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import Day01.*
import Day01Suite.*

class Day01Suite extends ScalaCheckSuite:

  test("day01 == 42"):
    assertEquals(day01, 42)

object Day01Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day01_input.txt")
