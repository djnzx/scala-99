package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p09]] */
object P09 {

  def pack[A](xs: List[A]): List[List[A]] = {

    @tailrec
    def go(xs: List[A], buf: List[A], outcome: List[List[A]]): List[List[A]] = (xs, buf) match {
      case (Nil, Nil)                  => Nil                             // first step, imput is empty
      case (Nil, _)                    => (buf :: outcome).reverse        // last step
      case (a :: at, Nil)              => go(at, List(a), outcome)        // first step, NON-EMPTY given
      case (a :: at, b :: _) if a == b => go(at, a :: buf, outcome)       // same char
      case (a :: at, _) /* xh != bh */ => go(at, List(a), buf :: outcome) // different char
    }

    go(xs, Nil, List.empty)
  }

}

class P09 extends Sandbox {
  import P09._

  test("1") {
    val data = Vector(
      ""     -> List(),
      "A"    -> List(List('A')),
      "AA"   -> List(List('A', 'A')),
      "AAA"  -> List(List('A', 'A', 'A')),
      "AAB"  -> List(List('A', 'A'), List('B')),
      "AABB" -> List(List('A', 'A'), List('B', 'B'))
    )

    for {
      (in, out) <- data
    } pack(in.toList) shouldBe out
  }
}
