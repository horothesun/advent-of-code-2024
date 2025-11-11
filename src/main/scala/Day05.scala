import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*

object Day05:

  case class Page(n: Int) derives Eq

  object Page:
    def parse(s: String): Option[Page] = s.toIntOption.map(Page.apply)

  case class OrderRule(before: Page, after: Page) derives Eq:
    def reverse: OrderRule = OrderRule(before = after, after = before)

  object OrderRule:
    def parse(s: String): Option[OrderRule] = s.split('|') match {
      case Array(before, after) => (Page.parse(before), Page.parse(after)).mapN(OrderRule.apply)
      case _                    => None
    }

  case class Update(pages: NonEmptyList[Page]):

    def allOrderRules: List[OrderRule] = pages.toList.zip(pages.tail).map(OrderRule.apply)

    def middlePage: Page =
      val midIndex = pages.length / 2
      pages.get(if pages.length % 2 == 0 then midIndex - 1 else midIndex).get

    def firstViolatedOrderRules(rules: NonEmptyList[OrderRule]): Option[OrderRule] =
      allOrderRules.map(_.reverse).find(rules.contains_)

  object Update:
    def parse(s: String): Option[Update] = s.split(',').toList.toNel.flatMap(_.traverse(Page.parse).map(Update.apply))

  extension [A](as: List[A])
    def splitBySeparator(sep: A): List[List[A]] =
      as.reverse.foldLeft(List(List.empty[A]))((l, a) => if a == sep then List.empty :: l else (a :: l.head) :: l.tail)

  case class Input(rules: NonEmptyList[OrderRule], updates: NonEmptyList[Update]):

    def filteredRules(update: Update): List[OrderRule] =
      rules.filter(r => update.pages.contains_(r.before) && update.pages.contains_(r.after))

    def correctlyOrderedUpdatesMiddlePageSum: Int =
      updates
        .collect(u => filteredRules(u).toNel.flatMap(u.firstViolatedOrderRules) match { case None => u })
        .foldMap(_.middlePage.n)

  object Input:
    def parse(rows: List[String]): Option[Input] =
      rows.splitBySeparator("") match {
        case rules :: updates :: Nil =>
          (
            rules.toNel.flatMap(rulesNel => rulesNel.traverse(OrderRule.parse)),
            updates.toNel.flatMap(_.traverse(Update.parse))
          ).mapN(Input.apply)
        case _ => None
      }
