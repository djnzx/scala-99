package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p08]] */
object P08 {

  def compress[A](xs: List[A]): List[A] = {

    @tailrec
    def go(xs: List[A], cur: Option[A], outcome: List[A]): List[A] = (cur, xs) match {
      case (None, Nil)                   => Nil                           // first step, empty list, `cur` can be empty on the first step
      case (None, h :: ts)               => go(ts, Some(h), outcome)      // first step, NON-empty list
      case (Some(c), Nil)                => (c :: outcome).reverse        // last iteration, just add the buffer
      case (Some(c), h :: ts) if c == h  => go(ts, Some(h), outcome)      // non-last, skip the same char
      case (Some(c), h :: ts) /* c!=h */ => go(ts, Some(h), c :: outcome) // non-last, different, collect char
    }

    go(xs, None, List.empty)
  }

}

class P08 extends Sandbox {
  import P08._

  test("1") {
    Seq(
      ""       -> "",
      "a"      -> "a",
      "aa"     -> "a",
      "aab"    -> "ab",
      "aabb"   -> "ab",
      "aabba"  -> "aba",
      "aabbaa" -> "aba"
    ).foreach { case (in, out) =>
      compress(in.toList) shouldBe out.toList
    }
  }
}
