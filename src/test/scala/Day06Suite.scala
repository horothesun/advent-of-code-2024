import Day06.*
import Day06.Cell.*
import Day06.Tile.*
import Day06Suite.*
import cats.syntax.all.*
import munit.ScalaCheckSuite

class Day06Suite extends ScalaCheckSuite:

  test("big input parsed to something"):
    assert(Lab.parse(bigInput).isDefined)

  test("small input parsed correctly"):
    assertEquals(
      Lab.parse(smallInput),
      Lab(
        cells = List(
          List(e, e, e, e, o, e, e, e, e, e),
          List(e, e, e, e, e, e, e, e, e, o),
          List(e, e, e, e, e, e, e, e, e, e),
          List(e, e, o, e, e, e, e, e, e, e),
          List(e, e, e, e, e, e, e, o, e, e),
          List(e, e, e, e, e, e, e, e, e, e),
          List(e, o, e, e, u, e, e, e, e, e),
          List(e, e, e, e, e, e, e, e, o, e),
          List(o, e, e, e, e, e, e, e, e, e),
          List(e, e, e, e, e, e, o, e, e, e)
        )
      ).some
    )

  test("small input guard is at pos (6, 4) and facing up"):
    assertEquals(
      Lab.parse(smallInput).map(_.guard),
      Guard(Pos(row = 6, col = 4), Direction.Up).some
    )

  test("guard will visit 41 distinct positions in small input lab"):
    assertEquals(Lab.parse(smallInput).map(_.allDistinctGuardPositionsCount), 41.some)

  test("guard will visit 5_131 distinct positions in big input lab"):
    assertEquals(Lab.parse(bigInput).map(_.allDistinctGuardPositionsCount), 5_131.some)

object Day06Suite:

  val e: Cell = TileCell(Empty)
  val o: Cell = TileCell(Obstruction)
  val u: Cell = GuardCell(Direction.Up)

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day06_input.txt")

  val smallInput: List[String] = List(
    "....#.....",
    ".........#",
    "..........",
    "..#.......",
    ".......#..",
    "..........",
    ".#..^.....",
    "........#.",
    "#.........",
    "......#..."
  )
