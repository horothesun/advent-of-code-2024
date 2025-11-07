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

  type GridStore = Store[Pos, Option[Char]]

  enum WordCheckResult:
    case Found, NotFound

    def toInt: Int = this match {
      case Found    => 1
      case NotFound => 0
    }
    def toBoolean: Boolean = this match {
      case Found    => true
      case NotFound => false
    }

  object WordCheckResult:
    def from(isFound: Boolean): WordCheckResult = if isFound then Found else NotFound

  case class Grid(rows: List[List[Char]]):

    val toVectors: Vector[Vector[Char]] = rows.map(_.toVector).toVector

    val toStore: GridStore =
      Store(
        p => toVectors.get(p.row).flatMap(_.get(p.col)),
        s = Pos.zero
      )

    val allPositions: List[Pos] =
      rows.zipWithIndex.flatMap((row, rowIndex) => row.zipWithIndex.map((_, colIndex) => Pos(rowIndex, colIndex)))

    def allOccurrences(s: String): Int =
      val w = Word(s)
      (allWordChecks(w) ++ allWordChecks(w.reverse)).foldMap(_.toInt)

    def allWordChecks(w: Word): List[WordCheckResult] =
      toStore
        .coflatMap(wordChecks(w, _))
        .experiment(_ => allPositions)
        .flatten

    // part 2
    def allCrossOccurrences(s: String): Int =
      toStore
        .coflatMap(crossWordCheck(Word(s), _))
        .experiment(_ => allPositions)
        .foldMap(_.toInt)

  object Grid:

    def parse(rows: List[String]): Option[Grid] = Grid(rows = rows.map(_.toList)).some

    def wordChecks(w: Word, store: GridStore): List[WordCheckResult] =
      List(
        horizontalPositions,
        verticalPositions,
        ascDiagonalPositions,
        descDiagonalPositions
      ).map(positions => wordCheck(positions(w.length), w, store))

    def wordCheck(positions: Pos => List[Pos], w: Word, store: GridStore): WordCheckResult =
      WordCheckResult.from(isFound = w.toOptionalChars == store.experiment(positions))

    /* XMAS
       ....
       ....
       .... */
    def horizontalPositions(wordLength: Int)(from: Pos): List[Pos] =
      List.range(from.col, from.col + wordLength).map(col => Pos(from.row, col))

    /* X...
       M...
       A...
       S... */
    def verticalPositions(wordLength: Int)(from: Pos): List[Pos] =
      List.range(from.row, from.row + wordLength).map(row => Pos(row, from.col))

    /* ...S
       ..A.
       .M..
       X... */
    def ascDiagonalPositions(wordLength: Int)(from: Pos): List[Pos] =
      List.range(0, wordLength).reverse.map(i => Pos(row = i + from.row, col = wordLength - i - 1 + from.col))

    /* X...
       .M..
       ..A.
       ...S */
    def descDiagonalPositions(wordLength: Int)(from: Pos): List[Pos] =
      List.range(0, wordLength).map(i => Pos(row = i + from.row, col = i + from.col))

    // part 2
    def crossWordCheck(w: Word, store: GridStore): WordCheckResult =
      def isAscDiagonalFound: Word => Boolean = wordCheck(ascDiagonalPositions(w.length), _, store).toBoolean
      def isDescDiagonalFound: Word => Boolean = wordCheck(descDiagonalPositions(w.length), _, store).toBoolean
      WordCheckResult.from(isFound =
        (isAscDiagonalFound(w) || isAscDiagonalFound(w.reverse)) &&
          (isDescDiagonalFound(w) || isDescDiagonalFound(w.reverse))
      )
