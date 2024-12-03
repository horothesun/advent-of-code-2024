import cats.data.NonEmptyList
import cats.parse.{Parser, Parser0}
import cats.parse.Numbers.nonNegativeIntString
import cats.parse.Parser.{char, end, string, until, until0, Error}
import cats.syntax.all.*

object Day03:

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
