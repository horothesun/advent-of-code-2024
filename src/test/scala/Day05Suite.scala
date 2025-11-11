import Day05.*
import Day05Suite.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import munit.ScalaCheckSuite

class Day05Suite extends ScalaCheckSuite:

  test("big input parsed to something"):
    assert(Input.parse(bigInput).isDefined)

  test("small input parsed to something"):
    assert(Input.parse(smallInput).isDefined)

  test("List splitBySeparator example"):
    assertEquals(
      List(1, 2, 3, 100, 4, 5, 100, 6).splitBySeparator(100),
      List(List(1, 2, 3), List(4, 5), List(6))
    )

  test("all order rules from update example"):
    assertEquals(
      Update(pages = NonEmptyList.of(1, 2, 3, 4).map(Page.apply)).allOrderRules,
      List(
        OrderRule(before = Page(1), after = Page(2)),
        OrderRule(before = Page(2), after = Page(3)),
        OrderRule(before = Page(3), after = Page(4))
      )
    )

  test("middle page from update with even # of pages example"):
    assertEquals(
      Update(pages = NonEmptyList.of(1, 2, 3, 4).map(Page.apply)).middlePage,
      Page(2)
    )

  test("middle page from update with odd # of pages example"):
    assertEquals(
      Update(pages = NonEmptyList.of(1, 2, 3, 4, 5).map(Page.apply)).middlePage,
      Page(3)
    )

  test("middle page from update with only one page example"):
    assertEquals(
      Update(pages = NonEmptyList.of(1).map(Page.apply)).middlePage,
      Page(1)
    )

  test("first violated order rule for small input's 1st update is None"):
    assertEquals(
      for {
        input <- Input.parse(smallInput)
        firstPage = input.updates.head
        orderRule <- firstPage.firstViolatedOrderRules(input.rules)
      } yield orderRule,
      None
    )

  test("first violated order rule for small input's 2nd update is None"):
    assertEquals(
      for {
        input <- Input.parse(smallInput)
        secondPage <- input.updates.toNev.get(1)
        orderRule <- secondPage.firstViolatedOrderRules(input.rules)
      } yield orderRule,
      None
    )

  test("first violated order rule for small input's 3rd update is None"):
    assertEquals(
      for {
        input <- Input.parse(smallInput)
        thirdPage <- input.updates.toNev.get(2)
        orderRule <- thirdPage.firstViolatedOrderRules(input.rules)
      } yield orderRule,
      None
    )

  test("first violated order rule for small input's 4th update is 97|75"):
    assertEquals(
      for {
        input <- Input.parse(smallInput)
        fourthPage <- input.updates.toNev.get(3)
        orderRule <- fourthPage.firstViolatedOrderRules(input.rules)
      } yield orderRule,
      OrderRule(before = Page(97), after = Page(75)).some
    )

  test("first violated order rule for small input's 5th update is 29|13"):
    assertEquals(
      for {
        input <- Input.parse(smallInput)
        fifthPage <- input.updates.toNev.get(4)
        orderRule <- fifthPage.firstViolatedOrderRules(input.rules)
      } yield orderRule,
      OrderRule(before = Page(29), after = Page(13)).some
    )

  test("first violated order rule for small input's 6th update is 75|13"):
    assertEquals(
      for {
        input <- Input.parse(smallInput)
        sixthPage <- input.updates.toNev.get(5)
        orderRule <- sixthPage.firstViolatedOrderRules(input.rules)
      } yield orderRule,
      OrderRule(before = Page(75), after = Page(13)).some
    )

  test("small input correctly ordered updates middle page sum is 143"):
    assertEquals(
      Input.parse(smallInput).map(_.correctlyOrderedUpdatesMiddlePageSum),
      143.some
    )

  test("big input correctly ordered updates middle page sum is 5_064"):
    assertEquals(
      Input.parse(bigInput).map(_.correctlyOrderedUpdatesMiddlePageSum),
      5_064.some
    )

object Day05Suite:

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day05_input.txt")

  val smallInput: List[String] = List(
    "47|53",
    "97|13",
    "97|61",
    "97|47",
    "75|29",
    "61|13",
    "75|53",
    "29|13",
    "97|29",
    "53|29",
    "61|53",
    "97|53",
    "61|29",
    "47|13",
    "75|47",
    "97|75",
    "47|61",
    "75|61",
    "47|29",
    "75|13",
    "53|13",
    "",
    "75,47,61,53,29",
    "97,61,53,29,13",
    "75,29,13",
    "75,97,47,61,53",
    "61,13,29",
    "97,13,75,29,47"
  )
