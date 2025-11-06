import Day04.*
import Day04Suite.*
import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class Day04Suite extends ScalaCheckSuite:

  test("small input parsed correctly"):
    assertEquals(
      Grid.parse(rows = List("abc", "def")),
      Grid(rows = List(List('a', 'b', 'c'), List('d', 'e', 'f'))).some
    )

  test("big input parsed to something"):
    assert(Grid.parse(rows = bigInput).isDefined)

  test("Grid allPositions example"):
    assertEquals(
      Grid(rows = List(List('a', 'b', 'c'), List('d', 'e', 'f'))).allPositions,
      List(
        Pos(row = 0, col = 0),
        Pos(row = 0, col = 1),
        Pos(row = 0, col = 2),
        Pos(row = 1, col = 0),
        Pos(row = 1, col = 1),
        Pos(row = 1, col = 2)
      )
    )

  property("Grid |allPositions| = rows * columns"):
    forAll(nonEmptyGridGen)(neg => assertEquals(neg.allPositions.length, neg.rows.length * neg.rows.head.length))

  test("topLeftPositions example"):
    assertEquals(
      Grid.topLeftPositions(Word("XYZ"))(Pos(row = 2, col = 4)),
      List(
        Pos(row = 2, col = 4),
        Pos(row = 2, col = 5),
        Pos(row = 2, col = 6)
      )
    )

  test("ascDiagonalPositions example"):
    assertEquals(
      Grid.ascDiagonalPositions(Word("XYZ"))(Pos(row = 2, col = 4)),
      List(
        Pos(row = 4, col = 4),
        Pos(row = 3, col = 5),
        Pos(row = 2, col = 6)
      )
    )

  test("descDiagonalPositions example"):
    assertEquals(
      Grid.descDiagonalPositions(Word("XYZ"))(Pos(row = 2, col = 4)),
      List(
        Pos(row = 2, col = 4),
        Pos(row = 3, col = 5),
        Pos(row = 4, col = 6)
      )
    )

object Day04Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day04_input.txt")

  val nonEmptyGridGen: Gen[Grid] = Gen.zip(Gen.choose(1, 10), Gen.choose(1, 10)).flatMap { (r, c) =>
    Gen.listOfN(r, Gen.listOfN(c, Gen.alphaUpperChar)).map(Grid.apply)
  }
