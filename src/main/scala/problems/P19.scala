package problems

import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p19]] */
object P19 {
  import P17.splitAt

  def rotate[A](n: Int, xs: List[A]): List[A] = n match {
    case 0 => xs
    case n =>
      val l = xs.length
      val nl = n % l
      //            right             left
      val at = if (nl > 0) l - nl else -nl
      val (lp, rp) = splitAt(at, xs)
      rp ::: lp
  }

}

class P19 extends Sandbox {
  import P19._

  test("1") {
    val data = Seq(
      (0, "abc")          -> "abc",
      (1, "abc")          -> "cab",
      (-1, "abc")         -> "bca",
      (4, "abc")          -> "cab",
      (1000000000, "abc") -> "cab",
      (-4, "abc")         -> "bca",
      (3, "abcdefghi")    -> "ghiabcdef",
      (-3, "abcdefghi")   -> "defghiabc"
    )

    for {
      ((n, in), out) <- data
    } yield rotate(n, in.toList) shouldEqual out.toList
  }

}
