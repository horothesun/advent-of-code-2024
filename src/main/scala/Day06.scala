import Day06.Cell.*
import Day06.Direction.*
import cats.data.Store
import cats.syntax.all.*

object Day06:

  case class Pos(row: Int, col: Int):

    def left: Pos = Pos(row, col - 1)
    def up: Pos = Pos(row - 1, col)
    def right: Pos = Pos(row, col + 1)
    def down: Pos = Pos(row + 1, col)

    def next(direction: Direction): Pos = direction match {
      case Left  => left
      case Up    => up
      case Right => right
      case Down  => down
    }

  object Pos:
    val outOfBounds: Pos = Pos(row = -1, col = -1)

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

  object Tile:
    def parse(c: Char): Option[Tile] = c match {
      case '.' => Empty.some
      case '#' => Obstruction.some
      case _   => None
    }

  enum Cell:
    case TileCell(tile: Tile)
    case GuardCell(direction: Direction)

    def toTile: Tile = this match {
      case TileCell(tile) => tile
      case GuardCell(_)   => Tile.Empty
    }

  object Cell:
    def parse(c: Char): Option[Cell] =
      Tile.parse(c).map(TileCell.apply).orElse(Direction.parse(c).map(GuardCell.apply))

  type LabStore = Store[Pos, Option[(Tile, Guard)]]

  val unguardedLabStore: LabStore =
    Store(
      {
        case Pos(row = 0, col = 0) => (Tile.Empty, Guard.outOfBounds).some
        case _                     => None
      },
      s = Pos.outOfBounds
    )

  extension (store: LabStore)

    def getGuard: Option[Guard] = store.extract.map(_._2)

    def afterGuardStep: LabStore =
      (for {
        oldGuard <- store.getGuard
        newGuard <- oldGuard.afterStep(store)
        newStore = store.seek(newGuard.pos).map(_.map((tile, _) => (tile, newGuard)))
      } yield newStore).getOrElse(unguardedLabStore)

  case class Guard(pos: Pos, direction: Direction):

    def afterStep(store: LabStore): Option[Guard] =
      val nextPos = pos.next(direction)
      store
        .peek(nextPos)
        .map(_._1 match {
          case Tile.Empty       => Guard(nextPos, direction)
          case Tile.Obstruction => Guard(pos, direction.turnedRight)
        })

  object Guard:
    val outOfBounds: Guard = Guard(Pos.outOfBounds, Direction.Left)

  case class Lab(cells: List[List[Cell]]):

    val toVectors: Vector[Vector[Cell]] = cells.map(_.toVector).toVector

    val guard: Guard =
      cells.zipWithIndex
        .map((row, rowIndex) => row.zipWithIndex.map((cell, colIndex) => (Pos(rowIndex, colIndex), cell)))
        .collectFirstSome(_.collectFirst { case (pos, GuardCell(direction)) => Guard(pos, direction) })
        .getOrElse(Guard.outOfBounds)

    val toStore: LabStore =
      Store(
        p => toVectors.get(p.row).flatMap(_.get(p.col).map(_.toTile)).map((_, guard)),
        s = guard.pos
      )

    def allDistinctGuardPositionsCount: Int =
      Set
        .unfold(init = toStore)(store => store.getGuard.map(guard => (guard.pos, store.afterGuardStep)))
        .size

  object Lab:
    def parse(rows: List[String]): Option[Lab] = rows.traverse(_.toList.traverse(Cell.parse)).map(Lab.apply)
