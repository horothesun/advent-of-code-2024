import cats.syntax.all.*

object Day01:

  case class LocationId(value: Int)

  object LocationId:
    given Ordering[LocationId] = Ordering.by(_.value)

  case class Input(left: List[LocationId], right: List[LocationId]):
    def totalDistance: Long = left.sorted.zip(right.sorted).map((l, r) => Math.abs(l.value - r.value).toLong).sum

  object Input:
    def parse(inputs: List[String]): Option[Input] =
      inputs.traverse {
        _.split(" {3}") match
          case Array(l, r) => (l.toIntOption, r.toIntOption).tupled
          case _           => None
      }.map { lrs =>
        Input(
          left = lrs.map((l, _) => LocationId(l)),
          right = lrs.map((_, r) => LocationId(r))
        )
      }

  def totalDistance(inputs: List[String]): Option[Long] = Input.parse(inputs).map(_.totalDistance)
