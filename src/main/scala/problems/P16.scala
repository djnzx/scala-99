package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p16]] */
object P16 {

  def dropNthR[A](n: Int, xs: List[A]): List[A] = {

    def go(i: Int, xs: List[A]): List[A] = (xs, i) match {
      case (Nil, _)    => Nil
      case (_ :: t, 1) => go(n, t)
      case (h :: t, c) => h :: go(c - 1, t)
    }

    go(n, xs)
  }

  def dropNthTR[A](n: Int, xs: List[A]) = {

    @tailrec
    def go(i: Int, xs: List[A], outcome: List[A]): List[A] = (xs, i) match {
      case (Nil, _)    => outcome.reverse
      case (_ :: t, 1) => go(n, t, outcome)
      case (h :: t, c) => go(c - 1, t, h :: outcome)
    }

    go(n, xs, Nil)
  }

}

class P16 extends Sandbox {

  import P16._

  test("1") {
    val impls = Seq(
      dropNthR[Char] _,
      dropNthTR[Char] _
    )

    val data = Vector(
      (1, "")            -> "",
      (1, "abc")         -> "",
      (2, "abcdef")      -> "ace",
      (3, "abcdefghijk") -> "abdeghjk"
    )

    for {
      impl           <- impls
      ((n, in), out) <- data
    } impl(n, in.toList) shouldBe out.toList

  }
}
