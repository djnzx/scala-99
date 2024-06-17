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

  case class Shape(sh: List[(Int, Int)])
  case class Shaped[A](value: A, shape: Shape)

  def combine(ls: Shape, rs: Shape): Shape = {
    val l: List[(Int, Int)] = ls.sh
    val r: List[(Int, Int)] = rs.sh

    ???
  }

  def shape(n: Tree[_]): Shape = n match {
    case End           => Shape(List.empty)
    case Node(_, l, r) => combine(shape(l), shape(r))
  }

  def join(l: List[(Int, Int)], r: List[(Int, Int)]): List[(Int, Int)] = {
    val lmax = l.map { case (_, r) => r }.maxOption.getOrElse(0)
    val rmax = r.map { case (l, _) => l }.minOption.getOrElse(0)
    val ldist = math.abs(lmax)
    val rdist = math.abs(rmax)
    pprint.log(lmax)
    pprint.log(rmax)
    val mx = lmax max -rmax // 1
    val shift = mx + 1

    (0, 0) ::
      (l zip r)
        .map { case ((ll, _), (_, rr)) => (-shift + ll, shift + rr) }
  }

  def layout3[A](t: Tree[A]): Tree[At[A]] = ???

}

class P66 extends Sandbox {
  import P57._
  import P66._

  test("join - plain 1") {
    val l = List(
      0  -> 0,
      -1 -> 1
    )
    val r = List(
      0  -> 0,
      -1 -> 1
    )
    val x: List[(Int, Int)] = join(l, r)
    pprint.log(x)
    x shouldBe List(
      0  -> 0,
      -2 -> 2,
      -3 -> 3
    )

  }

  val sample = bstFromList(List('n', 'k', 'm', 'c', 'a', 'e', 'd', 'g', 'u', 'p', 'q'))

  test("layout3") {
    val l = layout3(sample)
    pprint.log(l)
  }

}
