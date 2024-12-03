import Day02.Safety.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import fs2.Stream

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

    def safety: Safety = Report.safety(levels.toList)

    def safetyWithProblemDampener: Safety =
      Stream
        .range(start = -1, stopExclusive = levels.length)
        .map(elementAt => Report.safety(levels.removed(elementAt)))
        .collectFirst[Safety] { case s @ Safe => s }
        .toList
        .headOption
        .getOrElse(Unsafe)

  object Report:

    def parse(s: String): Option[Report] = s.split(' ').toList.traverse(Level.parse).flatMap(_.toNel.map(Report.apply))

    def isAllIncreasing(ls: List[Level]): Boolean = ls == ls.sorted
    def isAllDecreasing(ls: List[Level]): Boolean = ls == ls.sorted.reverse
    def areAllDeltasBetween1And3(ls: List[Level]): Boolean = ls match
      case Nil | _ :: Nil => true
      case _ :: tail      => ls.zip(tail).forall((l, r) => l.differsFrom(min = 1, max = 3)(r))

    def safety(ls: List[Level]): Safety =
      if ((isAllIncreasing(ls) || isAllDecreasing(ls)) && areAllDeltasBetween1And3(ls)) Safe else Unsafe

  extension [A](nel: NonEmptyList[A])
    def removed(elementAt: Int): List[A] =
      val as = nel.toList
      if (elementAt < 0 || nel.length <= elementAt) as
      else // 0 <= idx && idx < nel.length
        as.take(elementAt) ++ as.drop(1 + elementAt)

  def parse(input: List[String]): Option[List[Report]] = input.traverse(Report.parse)

  def safetyCount(input: List[String]): Option[Int] =
    parse(input).map(_.map(_.safety).collect { case s @ Safe => s }.length)

  def safetyWithProblemDampenerCount(input: List[String]): Option[Int] =
    parse(input).map(_.map(_.safetyWithProblemDampener).collect { case s @ Safe => s }.length)
