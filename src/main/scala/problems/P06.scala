package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p06]] */
object P06 {

  def isPalindrome[A](as: List[A]): Boolean = {
    import P05.reverseAndLen

    @tailrec
    def go(n: Int, xs: List[A], ys: List[A]): Boolean = (n, xs, ys) match {
      case (0, _, _)                           => true
      case (n, ah :: at, bh :: bt) if ah == bh => go(n - 1, at, bt)
      case _                                   => false
    }

    val (asr, len) = reverseAndLen(as)
    go(len / 2, as, asr)
  }

}

class P06 extends Sandbox {
  import P06._

  test("should work") {
    val t = List(
      List(1, 2, 3, 3, 2, 1),
      List(1, 2, 5, 2, 1),
      List(1),
      List.empty
    ).map(_ -> true).toMap

    val f = List(
      List(1, 1, 2, 3, 5, 8),
      List(1, 2, 5, 6, 2, 1)
    ).map(_ -> false).toMap

    (t ++ f).foreach { case (in, out) =>
      isPalindrome(in) shouldEqual out
    }
  }

}
