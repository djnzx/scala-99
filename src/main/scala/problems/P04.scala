package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p04]] */
object P04 {

  def lengthR[A](as: List[A]): Int = as match {
    case Nil    => 0
    case _ :: t => 1 + lengthR(t)
  }

  def lengthTR[A](as: List[A]): Int = {

    @tailrec
    def go(as: List[A], acc: Int): Int = as match {
      case Nil    => acc
      case _ :: t => go(t, acc + 1)
    }

    go(as, 0)
  }

}

class P04 extends Sandbox {

  import P04._

  test("should return 0 on empty list - tail recursive") {
    lengthTR(List.empty[Int]) shouldEqual 0
  }

  test("should return 0 on empty list - head recursive") {
    lengthR(List.empty[Int]) shouldEqual 0
  }

  test("should return length of non-empty list - tail recursive") {
    lengthTR(List(1, 1, 2, 3, 5, 8)) shouldEqual 6
  }

  test("should return length of non-empty list - head recursive") {
    lengthR(List(1, 1, 2, 3, 5, 8)) shouldEqual 6
  }

}
