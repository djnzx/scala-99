package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p20]] */
object P20 {

  def extractAt[A](n: Int, xs: List[A]) = {

    @tailrec
    def go(cnt: Int, xs: List[A], ys: List[A]): (List[A], A) = xs match {
      case x :: xs if cnt < n => go(cnt + 1, xs, x :: ys)
      case x :: xs            => (ys.reverse ::: xs, x)
      case Nil                => throw new NoSuchElementException
    }

    go(0, xs, Nil)
  }

}

class P20 extends Sandbox {
  import P20._

  test("good") {
    val data = Seq(
      (0, "AB")    -> ("B", 'A'),
      (1, "AB")    -> ("A", 'B'),
      (1, "ABCDE") -> ("ACDE", 'B'),
      (4, "ABCDE") -> ("ABCD", 'E')
    )

    for {
      ((n, in), (out1, out2)) <- data
    } extractAt(n, in.toList) shouldEqual (out1.toList, out2)

  }

  test("bad") {
    val ex = Seq(
      (0, ""),
      (5, "abcd")
    )

    for {
      (n, in) <- ex
    } an[NoSuchElementException] should be thrownBy extractAt(n, in.toList)

  }

}
