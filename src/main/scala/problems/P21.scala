package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p21]] */
object P21 {

  def insertAt[A](a: A, n: Int, xs: List[A]) = {

    @tailrec
    def go(cnt: Int, xs: List[A], acc: List[A]): List[A] = xs match {
      case Nil if cnt == n    => (a :: acc).reverse               // done
      case Nil                => throw new NoSuchElementException // list is to short
      case h :: t if cnt == n => (h :: a :: acc).reverse ::: t    // done
      case h :: t /*cnt < n*/ => go(cnt + 1, t, h :: acc)         // collect
    }

    go(0, xs, Nil)
  }

  /** it can be implemented in terms of split */
  def insertAt2[A](a: A, n: Int, xs: List[A]) = {
    import P17._

    val (l, r) = splitAt(n, xs)
    l ::: a :: r
  }

}

class P21 extends Sandbox {
  import P21._

  test("1") {
    val data = Seq(
      ("ABC", 0, 'X') -> "XABC",
      ("ABC", 1, 'X') -> "AXBC",
      ("ABC", 2, 'X') -> "ABXC",
      ("ABC", 3, 'X') -> "ABCX"
    )
    val datax = Seq(
      ("AB", 3, 'X')
    )
    for {
      ((into, at, what), out) <- data
    } yield {
      insertAt(what, at, into.toList) shouldEqual out.toList
      insertAt2(what, at, into.toList) shouldEqual out.toList
    }

    for {
      (into, at, what) <- datax
    } an[NoSuchElementException] should be thrownBy insertAt(what, at, into.toList)
  }
}
