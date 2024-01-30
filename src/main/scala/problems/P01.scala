package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p01]] */
object P01 {

  /** the function is partial, it fails on empty list */
  @tailrec
  def last[A](as: List[A]): A = as match {
    case Nil      => throw new NoSuchElementException
    case h :: Nil => h
    case _ :: t   => last(t)
  }

}

class P01 extends Sandbox {

  import P01._

  test("should find the last element") {
    val data = List(1, 1, 2, 3, 5, 8)
    last(data) shouldBe 8
  }

  test("should throw an Exception") {
    val empty = List.empty[Int]
    a[NoSuchElementException] should be thrownBy last(empty)
  }

}
