import Day06.Cell.*
import Day06.Direction.*
import cats.data.Store
import cats.derived.*
import cats.syntax.all.*

object Day06:

  case class Pos(row: Int, col: Int):

    def left: Pos = Pos(row, col - 1)
    def up: Pos = Pos(row - 1, col)
    def right: Pos = Pos(row, col + 1)
    def down: Pos = Pos(row + 1, col)

    def next(dir: Direction): Pos = dir match {
      case Left  => left
      case Up    => up
      case Right => right
      case Down  => down
    }

  enum Direction:
    case Left, Up, Right, Down

    def turnedRight: Direction = this match {
      case Left  => Up
      case Up    => Right
      case Right => Down
      case Down  => Left
    }

  object Direction:
    def parse(c: Char): Option[Direction] = c match {
      case '<' => Left.some
      case '^' => Up.some
      case '>' => Right.some
      case 'v' => Down.some
      case _   => None
    }

  enum Tile:
    case Empty, Obstruction

  enum Cell:
    case TileCell(tile: Tile)
    case GuardCell(direction: Direction)

  type LabWithNoGuardStore = Store[Pos, Option[Tile]]

  type LabStore = Store[Pos, Option[(Tile, Guard)]]

  def extractGuard(store: LabStore): Option[Guard] = store.extract.map(_._2)

  case class Guard(pos: Pos, direction: Direction):

    def updated(store: LabStore): Option[Guard] =
      val nextPos = pos.next(direction)
      store
        .peek(nextPos)
        .map(_._1 match {
          case Tile.Empty       => Guard(nextPos, direction)
          case Tile.Obstruction => Guard(pos, direction.turnedRight)
        })

  case class Lab(cells: List[List[Cell]], guard: Guard):

    val toVectors: Vector[Vector[Cell]] = cells.map(_.toVector).toVector

    val outOfBoundsPos: Pos = Pos(row = -1, col = -1)

    val toStoreWithNoGuard: LabWithNoGuardStore =
      Store(
        p =>
          toVectors
            .get(p.row)
            .flatMap(_.get(p.col).map {
              case TileCell(tile) => tile
              case GuardCell(_)   => Tile.Empty
            }),
        s = guard.pos
      )

    def withGuard(store: LabWithNoGuardStore): LabStore = store.map(_.map((_, guard)))

    def step(store: LabStore): LabStore =
      extractGuard(store).fold(ifEmpty = store) { oldGuard =>
        oldGuard
          .updated(store)
          .map(newGuard => store.seek(newGuard.pos).map(_.map((tile, _) => (tile, newGuard))))
          .getOrElse(store.seek(outOfBoundsPos)) // TODO: improve! 🔥🔥🔥
      }

    def allDistinctGuardPositions(store: LabWithNoGuardStore): List[Pos] =
      List
        .unfold(init = withGuard(store))(currentStore =>
          extractGuard(currentStore).map(guard => (guard.pos, step(currentStore)))
        )
        .distinct

  object Lab:
    def parse(rows: List[String]): Option[Lab] = ??? // rows.map(_.toList)
