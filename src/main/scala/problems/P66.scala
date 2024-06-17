package problems

import cats.implicits.catsSyntaxOptionId
import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p66]]
  *
  * [[https://aperiodic.net/pip/scala/s-99/p66.svg]]
  */
object P66 {
  import P55.Tree._
  import P55._
  import P64._

  def normalizeDistance(n: Int): Int = n match {
    case 0        => 2
    case IsOdd(n) => n + 1
    case n        => n + 2
  }

  /** put 2 trees as close as we can */
  def calculateOffset(ls: List[(Int, Int)], rs: List[(Int, Int)]): Int = {
    // analyze only common parts
    val both = (ls zip rs).map { case ((_, l), (r, _)) => l -> r }
    // analyze max RIGHT OFFSET for the LEFT PART
    val offsetMaxL = both.map(_._1).foldLeft(0)(_ max _)
    // analyze max LEFT OFFSET for the RIGHT PART
    val offsetMaxR = both.map(_._2).foldLeft(0)(_ max math.abs(_)) // left is negative
    // total width (minimal)
    val width = offsetMaxL + offsetMaxR
    // how close we can put them
    val distance = both                                            // offsets, each relates to its center
      .map { case (l, r) => (l, width + r) } // make offsets absolute coordinates (right is negative)
      .map { case lr @ (l, r) => (r - l) -> lr } // distance
      .minByOption { case (dist, _) => dist } // pick minimal distance
      .map { case (_, (lp, rp)) => lp + (width - rp) } // sum of offsets at minimal distance
      .map(normalizeDistance)
      .getOrElse(2)                                                // minimal distance by default

    distance / 2
  }

  def shiftAndCombine(xs: (Option[(Int, Int)], Option[(Int, Int)]), shift: Int): (Int, Int) =
    xs match {
      case (Some((l, _)), Some((_, r))) => (l - shift) -> (r + shift) // left part <=, right part =>
      case (Some((l, r)), None)         => (l - shift) -> (r - shift) // both parts <=
      case (None, Some((l, r)))         => (l + shift) -> (r + shift) // both parts =>
      case _                            => 0           -> 0           // never happen due to the nature of ZipAll
    }

  def combineA(ls: List[(Int, Int)], rs: List[(Int, Int)], offset: Int): List[(Int, Int)] =
    (0, 0) ::
      ls.map(_.some)
        .zipAll(rs.map(_.some), None, None)
        .map(shiftAndCombine(_, offset))

  case class Shape(a: List[(Int, Int)], shift: Int)
  object Shape {
    val empty = Shape(List.empty, 0)
  }

  def combine(ls: Shape, rs: Shape): Shape = {
    val offset = calculateOffset(ls.a, rs.a)
    val a = combineA(ls.a, rs.a, offset)
    Shape(a, offset)
  }

  // tests only
  def shape(n: Tree[_]): Shape = n match {
    case End           => Shape.empty
    case Node(_, l, r) => combine(shape(l), shape(r))
  }

  case class Shaped[A](value: A, shape: Shape)

  def analyze[A](t: Tree[A]): (Tree[Shaped[A]], Shape) = t match {
    case End               => End -> Shape.empty
    case Node(value, l, r) =>
      val (lt, ls) = analyze(l) // left branch + shape
      val (rt, rs) = analyze(r) // right branch + shape
      val s = combine(ls, rs)   // combine shapes
      val v = Shaped(value, s)  // build shaped value
      Node(v, lt, rt) -> s
  }

  def index[A](t: Tree[Shaped[A]], level: Int, xc: Int): Tree[At[A]] = t match {
    case End             => End
    case Node(shv, l, r) =>
      val at = At(shv.value, x = xc, y = level)
      val shift = shv.shape.shift
      val lt = index(l, level + 1, xc - shift)
      val rt = index(r, level + 1, xc + shift)
      Node(at, lt, rt)
  }

  def shiftOfTheMostLeft(t: Tree[Shaped[_]]): Int = t match {
    case End               => 0
    case Node(value, l, _) => value.shape.shift + shiftOfTheMostLeft(l)
  }

  def layout3[A](t: Tree[A]): Tree[At[A]] = {
    val (sh, _) = analyze(t)
    val center = shiftOfTheMostLeft(sh)
    index(sh, level = 1, xc = center)
  }

}

class P66 extends Sandbox {
  import P55._
  import P57._
  import P64._
  import P66._

  test("normalizeDistance") {
    val testData = Table(
      inOutHeader,
      0 -> 2,
      1 -> 2,
      2 -> 4,
      3 -> 4,
      4 -> 6,
      5 -> 6,
      6 -> 8,
      7 -> 8,
      8 -> 10,
    )

    forAll(testData) { case (in, out) =>
      normalizeDistance(in) shouldBe out
    }
  }

  test("combine 1 - plain") {
    val l = List(0 -> 0)
    val r = List(0 -> 0)
    val j = combineA(l, r, calculateOffset(l, r))
    j shouldBe List(
      0  -> 0,
      -1 -> 1,
    )
  }

  test("combine 2 - plain") {
    val l = List(
      0  -> 0,
      -1 -> 1
    )
    val r = List(
      0  -> 0,
      -1 -> 1
    )
    val j = combineA(l, r, calculateOffset(l, r))
    j shouldBe List(
      0  -> 0,
      -2 -> 2,
      -3 -> 3
    )
  }

  test("combine 3 - optimized") {
    val l = List(
      0 -> 0,
      1 -> 1
    )
    val r = List(
      0 -> 0,
      1 -> 1
    )
    val j = combineA(l, r, calculateOffset(l, r))
    j shouldBe List(
      0  -> 0,
      -1 -> 1,
      0  -> 2
    )
  }

  test("combine 4 - can't be optimized") {
    val l = List(
      0 -> 0,
      1 -> 1
    )
    val r = List(
      0  -> 0,
      -1 -> -1
    )
    val j = combineA(l, r, calculateOffset(l, r))
    j shouldBe List(
      0  -> 0,
      -2 -> 2,
      -1 -> 1
    )
  }

  test("combine 5 - optimized complicated") {
    val l = List(
      0 -> 0,
      1 -> 1,
      2 -> 2,
      3 -> 3,
    )
    val r = List(
      0 -> 0,
      1 -> 1,
      2 -> 2,
    )
    val j = combineA(l, r, calculateOffset(l, r))
    j shouldBe List(
      0  -> 0,
      -1 -> 1,
      0  -> 2,
      1  -> 3,
      2  -> 2,
    )
  }

  test("combine 6 - optimized zig-zag") {
    val l = List(
      0  -> 0,
      1  -> 1,
      0  -> 0,
      -1 -> -1,
      0  -> 0,
      1  -> 1,
      2  -> 2,
    )
    val r = List(
      0  -> 0,
      1  -> 1,
      0  -> 0,
      -1 -> -1,
      0  -> 0,
    )
    val j = combineA(l, r, calculateOffset(l, r))
    j shouldBe List(
      0  -> 0,
      -1 -> 1,
      0  -> 2,
      -1 -> 1,
      -2 -> 0,
      -1 -> 1,
      0  -> 0,
      1  -> 1,
    )
  }

  val sample = bstFromList(List('n', 'k', 'm', 'c', 'a', 'e', 'd', 'g', 'u', 'p', 'q'))

  test("shape") {
    val sh = shape(sample)
    sh.a shouldBe List((0, 0), (-2, 2), (-3, 1), (-4, 2), (-3, -1))
  }

  test("layout3") {
    val l3 = layout3(sample)
    val expected =
      Node(
        At('n', x = 5, y = 1),
        l = Node(
          At('k', x = 3, y = 2),
          l = Node(
            At('c', x = 2, y = 3),
            l = Node(At('a', x = 1, y = 4), l = End, r = End),
            r = Node(
              At('e', x = 3, y = 4),
              l = Node(At('d', x = 2, y = 5), l = End, r = End),
              r = Node(At('g', x = 4, y = 5), l = End, r = End)
            )
          ),
          r = Node(At('m', x = 4, y = 3), l = End, r = End)
        ),
        r = Node(
          At('u', x = 7, y = 2),
          l = Node(
            At('p', x = 6, y = 3),
            l = End,
            r = Node(At('q', x = 7, y = 4), l = End, r = End)
          ),
          r = End
        )
      )
    l3 shouldBe expected
  }

}
