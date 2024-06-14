package problems

import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p65]]
  *
  * [[https://aperiodic.net/pip/scala/s-99/p65.svg]]
  */
object P65 {
  import P55._
  import P64._

  def maxLevel(t: Tree[_]): Int = t match {
    case Node(_, l, r) => (maxLevel(l) max maxLevel(r)) + 1
    case End           => 0
  }

  def levelOfTheMostLeft(t: Tree[_]): Int = t match {
    case End           => 0
    case Node(_, l, _) => 1 + levelOfTheMostLeft(l)
  }

  def shiftOfTheMostLeft(t: Tree[_], shift: Int => Int): Int =
    (2 to levelOfTheMostLeft(t)).map(shift).sum

  def shiftAt(maxLevel: Int)(level: Int): Int = 1 << (maxLevel - level)

  def layout2[A](t: Tree[A]): Tree[At[A]] = {
    val shift: Int => Int = shiftAt(maxLevel(t))
    val offset = shiftOfTheMostLeft(t, shift)

    def go(t: Tree[A], xc: Int, lvl: Int): Tree[At[A]] = t match {
      case Node(value, l, r) =>
        val at = At(value, xc, lvl)                  // N - current level
        val lt = go(l, xc - shift(lvl + 1), lvl + 1) // L - next level
        val rt = go(r, xc + shift(lvl + 1), lvl + 1) // R - next level
        Node(at, lt, rt)
      case End               => End
    }

    go(t, xc = offset + 1, lvl = 1)
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

  test("levelOfTheMostLeft") {
    levelOfTheMostLeft(sample) shouldBe 4
  }

  test("base shift") {
    val shift = shiftAt(maxLevel(sample)) _
    shiftOfTheMostLeft(sample, shift) shouldBe 14
  }

  test("layout2") {
    val l = layout2(sample)
    pprint.log(l)
  }

}
