package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p17]] */
object P17 {

  /** 1-st assumption n <= xs.length */
  def splitAt[A](at: Int, xs: List[A]): (List[A], List[A]) = {

    @tailrec
    def go(n: Int, xs: List[A], ys: List[A]): (List[A], List[A]) = (xs, n) match {
      case (Nil, _) | (_, 0) => (ys.reverse, xs)
      case (h :: t, n)       => go(n - 1, t, h :: ys)
    }

    go(at, xs, Nil)
  }

  def splitAtR[A](at: Int, xs: List[A]): (List[A], List[A]) =
    (at, xs) match {
      case (0, _) | (_, Nil) => Nil -> xs
      case (n, x :: xs)      => splitAtR(n - 1, xs) match { case (l1, l2) => (x :: l1, l2) }
    }

}

class P17 extends Sandbox {
  import P17._

  test("1") {
    val data = Vector(
      (1, "")            -> ("", ""),
      (10, "")           -> ("", ""),
      (10, "abc")        -> ("abc", ""),
      (1, "abc")         -> ("a", "bc"),
      (2, "abcdef")      -> ("ab", "cdef"),
      (3, "abcdefghijk") -> ("abc", "defghijk")
    )

    for {
      ((n, in), (out1, out2)) <- data
    } yield {
      val expected = (out1.toList, out2.toList)
      splitAt(n, in.toList) shouldBe expected
      splitAtR(n, in.toList) shouldEqual expected
    }

  }
}
