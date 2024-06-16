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

  def join(l: List[(Int, Int)], r: List[(Int, Int)]): List[(Int, Int)] = {
    val lmax = l.maxBy { case (l, r) => r }._2
    val rmax = r.minBy { case (l, r) => l }._1
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

  test("join") {
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
