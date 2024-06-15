package problems

import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p65]]
  *
  * [[https://aperiodic.net/pip/scala/s-99/p65.svg]]
  */
object P65 {
  import P55._
  import P64._

  def maxLevel(t: Tree[_]): Int =
    t match {
      case Node(_, l, r) => (maxLevel(l) max maxLevel(r)) + 1
      case End           => 0
    }

  def shiftAt(maxLevel: Int)(level: Int): Int =
    1 << (maxLevel - level)

  def layout2[A](t: Tree[A]): Tree[At[A]] = {

    def go(t: Tree[A], x: Int, level: Int): (Tree[At[A]], Int) = t match {
      case Node(value, l, r) =>
        val (lt, x1) = go(l, x, level + 1)
        val (xn, x2) = (x1, x1 + 1)
        val (rt, x3) = go(r, x2, level + 1)
        val at = At(value, xn, level)
        Node(at, lt, rt) -> x3
      case End               => End -> x
    }

    go(t, x = 1, level = 1)._1
  }

}

class P65 extends Sandbox {
  import P55._
  import P57._
  import P65._

  val sample = bstFromList(List('n', 'k', 'm', 'c', 'a', 'e', 'd', 'g', 'u', 'p', 'q'))

  test("maxLevel") {
    val testData = Table(
      inOutHeader,
      End                                                 -> 0,
      Node(111, End, End)                                 -> 1,
      Node(111, Node(123, End, End), End)                 -> 2,
      Node(111, Node(123, End, End), Node(123, End, End)) -> 2,
      sample                                              -> 5
    )

    forAll(testData) { case (bst, level) =>
      maxLevel(bst) shouldBe level
    }

  }

  test("shiftAt") {
    val testData = Table(
      inOutHeader,
      (5, 1) -> 16,
      (5, 2) -> 8,
      (5, 3) -> 4,
      (5, 4) -> 2,
      (5, 5) -> 1,
    )

    forAll(testData) { case ((max, level), w) =>
      shiftAt(max)(level) shouldBe w
    }

  }

  test("layout") {
    val l = layout2(sample)
    pprint.log(l)
  }

}
