import cats.syntax.all.*

object Day01:

  case class LocationId(value: Int)

  object LocationId:
    given Ordering[LocationId] = Ordering.by(_.value)

  case class Input(left: List[LocationId], right: List[LocationId]):

    def totalDistance: Long = left.sorted.zip(right.sorted).foldMap((l, r) => Math.abs(l.value - r.value).toLong)

    def similarityScore: Long =
      val rightCount = right.groupBy(identity).map((k, vs) => (k, vs.length))
      left.foldMap(l => (l.value * rightCount.getOrElse(l, 0)).toLong)

  object Input:
    def parse(inputs: List[String]): Option[Input] =
      inputs.traverse {
        _.split(" {3}") match
          case Array(l, r) => (l.toIntOption, r.toIntOption).tupled
          case _           => None
      }.map { lrs =>
        val (l, r) = lrs.unzip
        Input(left = l.map(LocationId.apply), right = r.map(LocationId.apply))
      }

  def totalDistance(inputs: List[String]): Option[Long] = Input.parse(inputs).map(_.totalDistance)

  def similarityScore(inputs: List[String]): Option[Long] = Input.parse(inputs).map(_.similarityScore)
