import Day01.*
import Day01Suite.*
import munit.ScalaCheckSuite

class Day01Suite extends ScalaCheckSuite:

  test("parse small input"):
    assertEquals(
      Input.parse(smallInput),
      Some(
        Input(
          left = List(3, 4, 2, 1, 3, 3).map(LocationId.apply),
          right = List(4, 3, 5, 3, 9, 3).map(LocationId.apply)
        )
      )
    )

  test("total distance for small input is 11"):
    assertEquals(totalDistance(smallInput), Some(11L))

  test("total distance for big input is 1_223_326"):
    assertEquals(totalDistance(bigInput), Some(1_223_326L))

  test("similarity score for small input is 31"):
    assertEquals(similarityScore(smallInput), Some(31L))

  test("similarity score for big input is 21_070_419"):
    assertEquals(similarityScore(bigInput), Some(21_070_419L))

object Day01Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day01_input.txt")

  val smallInput: List[String] = List(
    "3   4",
    "4   3",
    "2   5",
    "1   3",
    "3   9",
    "3   3"
  )
