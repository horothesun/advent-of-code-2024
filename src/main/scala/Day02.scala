import Day02.Safety.*
import cats.data.NonEmptyList
import cats.syntax.all.*

object Day02:

  case class Level(value: Int):
    def differsFrom(min: Int, max: Int)(that: Level): Boolean =
      Math.abs(value - that.value) >= min && Math.abs(value - that.value) <= max

  object Level:

    given Ordering[Level] = Ordering.by(_.value)

    def parse(s: String): Option[Level] = s.toIntOption.map(Level.apply)

  enum Safety:
    case Safe, Unsafe

  case class Report(levels: NonEmptyList[Level]):

    def isAllIncreasing: Boolean = levels.toList == levels.toList.sorted
    def isAllDecreasing: Boolean = levels.toList == levels.toList.sorted.reverse
    def areAllDeltasBetween1And3: Boolean =
      levels.toList.zip(levels.tail).forall((l, r) => l.differsFrom(min = 1, max = 3)(r))

    def safety: Safety = if ((isAllIncreasing || isAllDecreasing) && areAllDeltasBetween1And3) Safe else Unsafe

  object Report:
    def parse(s: String): Option[Report] = s.split(' ').toList.traverse(Level.parse).flatMap(_.toNel.map(Report.apply))

  def parse(input: List[String]): Option[List[Report]] = input.traverse(Report.parse)

  def safetyCount(input: List[String]): Option[Int] =
    parse(input).map(_.foldMap(_.safety match
      case Safe   => 1
      case Unsafe => 0
    ))
