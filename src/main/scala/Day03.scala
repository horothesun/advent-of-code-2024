import Day03.Op.*
import Day03.SeparatedElement.*
import cats.data.NonEmptyList
import cats.parse.{Parser, Parser0}
import cats.parse.Numbers.nonNegativeIntString
import cats.parse.Parser.{char, end, oneOf, string, until, until0, Error}
import cats.syntax.all.*

object Day03:

  // part 1

  case class Mul(x: Int, y: Int):
    def eval: Int = x * y

  object Mul:
    def parser: Parser[Mul] =
      val natLt1k = nonNegativeIntString.collect { case s if 1 <= s.length && s.length <= 3 => s.toInt }
      val natLt1kPair = (
        char('(') *> natLt1k.repSep(sep = char(',')) <* char(')')
      ).collect { case NonEmptyList(x, y :: Nil) => (x, y) }
      string("mul") *> natLt1kPair.map(Mul.apply.tupled)

  val mulsOnlyParser: Parser0[List[Mul]] =
    val mulRep = Mul.parser.backtrack.rep.map(_.toList)
    until0(Mul.parser) *> mulRep.repSep0(sep = until(Mul.parser)).map(_.flatten) <* until0(end)

  def evalMulsOnly(inputs: List[String]): Either[Error, Int] =
    inputs.traverse(s => mulsOnlyParser.parseAll(s).map(_.foldMap(_.eval))).map(_.sum)

  // part 2

  sealed trait SeparatedElement[A, S]
  object SeparatedElement:
    case class Sep[A, S](s: S) extends SeparatedElement[A, S]
    case class Elem[A, S](a: A) extends SeparatedElement[A, S]

  extension [A, S](l: List[SeparatedElement[A, S]])
    def splitBySep(start: S): List[(Sep[A, S], List[Elem[A, S]])] =
      val (acc, last) =
        l.foldLeft[(List[(Sep[A, S], List[Elem[A, S]])], (Sep[A, S], List[Elem[A, S]]))](
          (List.empty, (Sep[A, S](start), List.empty))
        ) { case ((accElemOrSep, (sep, elems)), elemOrSep) =>
          elemOrSep match
            case newSep @ Sep(_)   => (accElemOrSep :+ (sep, elems), (newSep, List.empty))
            case newElem @ Elem(_) => (accElemOrSep, (sep, elems :+ newElem))
        }
      acc :+ last

  type Enabler = Do.type | DoNot.type

  sealed trait Op:
    def toSeparatedElement: SeparatedElement[Multiply, Enabler] = this match
      case op @ (Do | DoNot)    => Sep(op)
      case mul @ Multiply(_, _) => Elem(mul)

  object Op:

    case object Do extends Op:
      def parser: Parser[Do.type] = string("do()").as(Do)

    case object DoNot extends Op:
      def parser: Parser[DoNot.type] = string("don't()").as(DoNot)

    case class Multiply(x: Int, y: Int) extends Op:
      def eval: Int = x * y
    object Multiply:
      def parser: Parser[Multiply] =
        val natLt1k = nonNegativeIntString.collect { case s if 1 <= s.length && s.length <= 3 => s.toInt }
        val natLt1kPair = (
          char('(') *> natLt1k.repSep(sep = char(',')) <* char(')')
        ).collect { case NonEmptyList(x, y :: Nil) => (x, y) }
        string("mul") *> natLt1kPair.map(Multiply.apply.tupled)

    def parser: Parser[Op] = oneOf(List(Do.parser, DoNot.parser, Multiply.parser))

  def group(ops: List[Op]): List[(Enabler, List[Multiply])] =
    ops
      .map(_.toSeparatedElement)
      .splitBySep(start = Do)
      .map { case (Sep(sep), elems) => (sep, elems.map(_.a)) }

  def evalEnabledMulsOnlyFromOps(ops: List[Op]): Int =
    group(ops).collect { case (Do, muls) => muls }.flatten.foldMap(_.eval)

  val opsParser: Parser0[List[Op]] =
    val opRep = Op.parser.backtrack.rep.map(_.toList)
    until0(Op.parser) *> opRep.repSep0(sep = until(Op.parser)).map(_.flatten) <* until0(end)

  def evalEnabledMulsOnly(inputs: List[String]): Either[Error, Int] =
    val contiguousInput = inputs.mkString("")
    opsParser.parseAll(contiguousInput).map(evalEnabledMulsOnlyFromOps)
