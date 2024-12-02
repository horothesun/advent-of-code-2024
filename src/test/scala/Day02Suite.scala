import Day02.*
import Day02Suite.*
import cats.data.NonEmptyList
import munit.ScalaCheckSuite

class Day02Suite extends ScalaCheckSuite:

  test("parsing small input"):
    assertEquals(
      parse(smallInput),
      Some(
        List(
          NonEmptyList.of(7, 6, 4, 2, 1).map(Level.apply),
          NonEmptyList.of(1, 2, 7, 8, 9).map(Level.apply),
          NonEmptyList.of(9, 7, 6, 2, 1).map(Level.apply),
          NonEmptyList.of(1, 3, 2, 4, 5).map(Level.apply),
          NonEmptyList.of(8, 6, 4, 4, 1).map(Level.apply),
          NonEmptyList.of(1, 3, 6, 7, 9).map(Level.apply)
        ).map(Report.apply)
      )
    )

  test("parsing big input returns a proper reports list"):
    assert(parse(bigInput).isDefined)

  test("safety count for small input is 2"):
    assertEquals(safetyCount(smallInput), Some(2))

  test("safety count for big input is 490"):
    assertEquals(safetyCount(bigInput), Some(490))

object Day02Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day02_input.txt")

  val smallInput: List[String] = List(
    "7 6 4 2 1",
    "1 2 7 8 9",
    "9 7 6 2 1",
    "1 3 2 4 5",
    "8 6 4 4 1",
    "1 3 6 7 9"
  )
