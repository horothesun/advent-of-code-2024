import cats.data.Store
import cats.syntax.all.*

object Day04:

  case class Word(s: String):
    val length: Int = s.length
    val toOptionChars: List[Option[Char]] = s.toList.map(Some.apply)

  case class Pos(row: Int, col: Int)

  object Pos:
    val zero = Pos(row = 0, col = 0)

  type StoreGrid = Store[Pos, Option[Char]]

  enum WordCheckResult:
    case Found, NotFound

  case class Grid(rows: List[List[Char]]):

    val toVectors: Vector[Vector[Char]] = rows.map(_.toVector).toVector

    def toStore: StoreGrid = Store(
      p => toVectors.get(p.row).flatMap(_.get(p.col)),
      s = Pos.zero
    )

    def allPositions: List[Pos] =
      rows.zipWithIndex.flatMap((row, rowIndex) => row.zipWithIndex.map((_, colIndex) => Pos(rowIndex, colIndex)))

  object Grid:

    def parse(rows: List[String]): Option[Grid] = Grid(rows = rows.map(_.toList)).some

    /*
      XMAS
      ....
      ....
      ....
     */
    def topLeftPositions(w: Word)(p: Pos): List[Pos] =
      List.range(start = p.col, end = p.col + w.length).map(col => Pos(p.row, col))

    def wordCheckTopLeft(w: Word, store: StoreGrid): WordCheckResult = wordCheck(topLeftPositions, w, store)

    /*
      ...S
      ..A.
      .M..
      X...
     */
    def ascDiagonalPositions(w: Word)(p: Pos): List[Pos] =
      List.range(start = 0, end = w.length).reverse.map(i => Pos(row = i + p.row, col = w.length - i - 1 + p.col))

    def wordCheckAscDiagonal(w: Word, store: StoreGrid): WordCheckResult = wordCheck(ascDiagonalPositions, w, store)

    /*
      X...
      .M..
      ..A.
      ...S
     */
    def descDiagonalPositions(w: Word)(p: Pos): List[Pos] =
      List.range(start = 0, end = w.length).map(i => Pos(row = i + p.row, col = i + p.col))

    def wordCheckDescDiagonal(w: Word, store: StoreGrid): WordCheckResult = wordCheck(descDiagonalPositions, w, store)

    def wordCheck(positions: Word => Pos => List[Pos], w: Word, store: StoreGrid): WordCheckResult =
      if w.toOptionChars == store.experiment(positions(w)) then WordCheckResult.Found else WordCheckResult.NotFound
