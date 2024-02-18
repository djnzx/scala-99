package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p05]] */
object P05 {

  def reverse[A](as: List[A]): List[A] = {

    @tailrec
    def go(as: List[A], acc: List[A]): List[A] = as match {
      case Nil    => acc;
      case h :: t => go(t, h :: acc)
    }

    go(as, Nil)
  }

  def reverseAndLen[A](as: List[A]): (List[A], Int) = {

    @tailrec
    def go(as: List[A], acc: (List[A], Int)): (List[A], Int) = (as, acc) match {
      case (Nil, _)                => acc;
      case (h :: t, (list, count)) => go(t, (h :: list, count + 1))
    }

    go(as, (Nil, 0))
  }

}

class P05 extends Sandbox {
  import P05._

  test("should return empty list for empty list") {
    reverse(List.empty) shouldEqual List.empty
  }

  test("should return the same list") {
    reverse(List(42)) shouldEqual List(42)
  }

  test("should return reversed list") {
    reverse(List(7, 13, 42)) shouldEqual List(42, 13, 7)
  }

  test("reverse and len - empty") {
    reverseAndLen(List.empty[Int]) shouldEqual (List.empty[Int], 0)
  }

  test("reverse and len - singleton") {
    reverseAndLen(List(13)) shouldEqual (List(13), 1)
  }

  test("reverse and len - non-empty") {
    reverseAndLen(List(1, 5)) shouldEqual (List(5, 1), 2)
  }

}
