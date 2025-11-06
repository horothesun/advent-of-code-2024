import Day04.Grid.*
import cats.data.Store
import cats.syntax.all.*

object Day04:

  case class Word(s: String):
    val length: Int = s.length
    val toOptionalChars: List[Option[Char]] = s.toList.map(_.some)
    def reverse: Word = Word(s.reverse)

  case class Pos(row: Int, col: Int)

  object Pos:
    val zero = Pos(row = 0, col = 0)

  type StoreGrid = Store[Pos, Option[Char]]

  enum WordCheckResult:
    case Found, NotFound

  case class Grid(rows: List[List[Char]]):

    val toVectors: Vector[Vector[Char]] = rows.map(_.toVector).toVector

    val toStore: StoreGrid = Store(
      p => toVectors.get(p.row).flatMap(_.get(p.col)),
      s = Pos.zero
    )

    val allPositions: List[Pos] =
      rows.zipWithIndex.flatMap((row, rowIndex) => row.zipWithIndex.map((_, colIndex) => Pos(rowIndex, colIndex)))

    def allWordChecks(w: Word): List[WordCheckResult] =
      toStore
        .coflatMap(s =>
          List(
            horizontalPositions,
            verticalPositions,
            ascDiagonalPositions,
            descDiagonalPositions
          ).map(wordCheck(_)(w, s))
        )
        .experiment(_ => allPositions)
        .flatten

    def allOccurrences(s: String): Int =
      val w = Word(s)
      (allWordChecks(w) ++ allWordChecks(w.reverse)).foldMap {
        case WordCheckResult.Found    => 1
        case WordCheckResult.NotFound => 0
      }

  object Grid:

    def parse(rows: List[String]): Option[Grid] = Grid(rows = rows.map(_.toList)).some

    def wordCheck(positions: Word => Pos => List[Pos])(w: Word, store: StoreGrid): WordCheckResult =
      if w.toOptionalChars == store.experiment(positions(w)) then WordCheckResult.Found else WordCheckResult.NotFound

    /*
      XMAS
      ....
      ....
      ....
     */
    def horizontalPositions(w: Word)(from: Pos): List[Pos] =
      List.range(start = from.col, end = from.col + w.length).map(col => Pos(from.row, col))

    /*
      X...
      M...
      A...
      S...
     */
    def verticalPositions(w: Word)(from: Pos): List[Pos] =
      List.range(start = from.row, end = from.row + w.length).map(row => Pos(row, from.col))

    /*
      ...S
      ..A.
      .M..
      X...
     */
    def ascDiagonalPositions(w: Word)(from: Pos): List[Pos] =
      List.range(start = 0, end = w.length).reverse.map(i => Pos(row = i + from.row, col = w.length - i - 1 + from.col))

    /*
      X...
      .M..
      ..A.
      ...S
     */
    def descDiagonalPositions(w: Word)(from: Pos): List[Pos] =
      List.range(start = 0, end = w.length).map(i => Pos(row = i + from.row, col = i + from.col))
