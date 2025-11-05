import cats.data.Store
import cats.syntax.all.*

object Day04:

//  val s: Store[Int, Option[Char]] = Store(f = _ => Some('a'), s = 0)
//  val x: Option[Char] = s.peeks(identity)
//  val y: List[Option[Char]] = s.experiment[List](n => List.range(start = n, end = n + 3))

  case class Pos(row: Int, col: Int)

  object Pos:
    val zero = Pos(row = 0, col = 0)

  case class Grid(rows: List[List[Char]]):

    private val toVectors: Vector[Vector[Char]] = rows.map(_.toVector).toVector

    def toStore: Store[Pos, Option[Char]] = Store(
      p => toVectors.get(p.row).flatMap(_.get(p.col)),
      s = Pos.zero
    )

    def allPositions: List[Pos] =
      rows.zipWithIndex.flatMap((row, rowIndex) => row.zipWithIndex.map((_, colIndex) => Pos(rowIndex, colIndex)))

  object Grid:
    def parse(rows: List[String]): Option[Grid] = Grid(rows = rows.map(_.toList)).some
