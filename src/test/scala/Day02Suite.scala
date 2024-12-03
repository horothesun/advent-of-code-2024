import Day02.*
import Day02Suite.*
import cats.data.NonEmptyList
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

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

  test("NEL removed(elementAt = <validIndex>) sample"):
    assertEquals(
      NonEmptyList.of('a', 'b', 'c', 'd').removed(elementAt = 1),
      List('a', 'c', 'd')
    )

  property("NEL removed(elementAt = <validIndex>) result has NEL length - 1"):
    forAll(
      Gen
        .choose(min = 1, max = 100)
        .flatMap(length =>
          Gen.zip(
            nonEmptyListGen(Gen.alphaChar, length),
            Gen.choose(min = 0, max = length - 1)
          )
        )
    )((nel, validIndex) => assertEquals(nel.removed(elementAt = validIndex).length, nel.length - 1))

  property("NEL removed(elementAt = <invalidIndex>) preserves input list"):
    forAll(
      Gen
        .choose(min = 1, max = 100)
        .flatMap(length =>
          Gen.zip(
            nonEmptyListGen(Gen.alphaChar, length),
            Gen.oneOf(
              Gen.choose(min = -20, max = -1),
              Gen.choose(min = length, max = 20 + length)
            )
          )
        )
    )((nel, invalidIndex) => assertEquals(nel.removed(elementAt = invalidIndex), nel.toList))

  test("safety count w/ Problem Dampener for small input is 4"):
    assertEquals(safetyWithProblemDampenerCount(smallInput), Some(4))

  test("safety count w/ Problem Dampener for big input is 536"):
    assertEquals(safetyWithProblemDampenerCount(bigInput), Some(536))

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

  def nonEmptyListGen[A](aGen: Gen[A], length: Int): Gen[NonEmptyList[A]] =
    Gen.zip(aGen, Gen.listOfN(length - 1, aGen)).map(NonEmptyList.apply)
