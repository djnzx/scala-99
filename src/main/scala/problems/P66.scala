package problems

import cats.implicits.catsSyntaxOptionId
import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p66]]
  *
  * [[https://aperiodic.net/pip/scala/s-99/p66.svg]]
  */
object P66 {
  import P55._
  import P64._
  import P65._

  def normalizeDistance(n: Int): Int = n match {
    case 0                 => 2
    case n if (n & 1) == 1 => n + 1
    case n                 => n + 2
  }

  def calculateShift(ls: List[(Int, Int)], rs: List[(Int, Int)]): Int = {
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

  def combine(ls: List[(Int, Int)], rs: List[(Int, Int)]): List[(Int, Int)] = {
    val shift = calculateShift(ls, rs)

    (0, 0) ::
      ls.map(_.some)
        .zipAll(rs.map(_.some), None, None)
        .map(shiftAndCombine(_, shift))
  }

  case class Shape(sh: List[(Int, Int)])
  case class Shaped[A](value: A, shape: Shape)

  def shape(n: Tree[_]): Shape = n match {
    case End           => Shape(List.empty)
    case Node(_, l, r) => Shape(combine(shape(l).sh, shape(r).sh))
  }

  def enrich[A](t: Tree[A], level: Int, xc: Int = 0): Tree[At[A]] = t match {
    case End               => End
    case Node(value, l, r) =>
      val at = At(value, x = xc, y = level)
      // TODO: analyze
      val lt = enrich(l, level + 1, xc)
      val rt = enrich(r, level + 1, xc)
      Node(at, lt, rt)
  }

  // TODO: combine enrich + shape to use the details
  def layout0[A](t: Tree[A]): Tree[Shaped[A]] = ???

  def index[A](t: Tree[Shaped[A]], level: Int, xc: Int): Tree[At[A]] = t match {
    case End             => End
    case Node(shv, l, r) =>
      val at = At(shv.value, x = xc, y = level)
      val lt = index(l, level + 1, xc) // TODO: modify Shape to contain shift
      val rt = index(r, level + 1, xc) // TODO: modify Shape to contain shift
      Node(at, lt, rt)
  }

  def layout3[A](t: Tree[A]): Tree[At[A]] = {
    val sh = layout0(t)
    index(sh, level = 1, xc = 0)
  }

}

class P66 extends Sandbox {
  import P57._
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
    val j = combine(l, r)
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
    val j = combine(l, r)
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
    val j = combine(l, r)
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
    val j = combine(l, r)
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
    val j = combine(l, r)
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
    val j = combine(l, r)
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
    pprint.log(sh)
  }

  test("layout3") {
    val l = layout3(sample)
    pprint.log(l)
  }

}
