import cats.Eq
import cats.data.{NonEmptyList, NonEmptyVector}
import cats.derived.*
import cats.syntax.all.*
import scala.annotation.tailrec

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

    val toNev: NonEmptyVector[Page] = pages.toNev

    val toNevWithIndex: NonEmptyVector[(Page, Int)] = toNev.zipWithIndex

    def allOrderRules: List[OrderRule] = pages.toList.zip(pages.tail).map(OrderRule.apply)

    def middlePage: Page =
      val midIndex = pages.length / 2
      pages.get(if pages.length % 2 == 0 then midIndex - 1 else midIndex).get

    def firstViolatedOrderRules(rules: NonEmptyList[OrderRule]): Option[OrderRule] =
      allOrderRules.map(_.reverse).find(rules.contains_)

    def swap(p1: Page, p2: Page): Update =
      (toNevWithIndex.find(_._1 == p1), toNevWithIndex.find(_._1 == p2)).tupled.fold(ifEmpty = this) {
        case ((_, i1), (_, i2)) => Update(pages = toNev.updatedUnsafe(i1, p2).updatedUnsafe(i2, p1).toNonEmptyList)
      }

    @tailrec
    final def fixed(rules: NonEmptyList[OrderRule]): Update =
      firstViolatedOrderRules(rules) match {
        case Some(r) => swap(r.before, r.after).fixed(rules)
        case None    => this
      }

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

    def fixedIncorrectlyOrderedUpdatesMiddlePageSum: Int =
      updates.collect { u =>
        filteredRules(u).toNel.flatMap(rs => u.firstViolatedOrderRules(rs).map((rs, _))) match {
          case Some((rulesForUpdate, _)) => (u, rulesForUpdate)
        }
      }.map((u, rulesForUpdate) => u.fixed(rulesForUpdate))
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
