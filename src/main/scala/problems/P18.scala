package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p18]] */
object P18 {

  /** including i, excluding k */
  def sliceAtR[A](i: Int, k: Int, xs: List[A]) = {

    def doSlice(idx: Int, xs: List[A]): List[A] = xs match {
      case _ :: xs if idx < i => doSlice(idx + 1, xs)      // skip
      case x :: xs if idx < k => x :: doSlice(idx + 1, xs) // take
      case _                  => Nil                       // done
    }

    doSlice(0, xs)
  }

  def sliceAtTR[A](i: Int, k: Int, xs: List[A]) = {

    @tailrec
    def go(idx: Int, xs: List[A], outcome: List[A]): List[A] = xs match {
      case _ :: t if idx < i => go(idx + 1, t, outcome)      // skip
      case h :: t if idx < k => go(idx + 1, t, h :: outcome) // take
      case _                 => outcome.reverse              // done
    }

    go(0, xs, Nil)
  }

}

class P18 extends Sandbox {

  import P18._

  test("1") {
    val impls = Seq(
      sliceAtR[Char] _,
      sliceAtTR[Char] _
    )

    val data = Seq(
      (3, 7, "")           -> "",
      (0, 0, "abc")        -> "",
      (0, 1, "abc")        -> "a",
      (0, 2, "abc")        -> "ab",
      (3, 7, "qwertyuiop") -> "rtyu",
      (3, 7, "qwer")       -> "r"
    )

    for {
      impl                  <- impls
      ((from, to, in), out) <- data
    } impl(from, to, in.toList) shouldEqual out.toList

  }
}
