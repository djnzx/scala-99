package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p02]] */
object P02 {

  @tailrec
  def penultimate[A](as: List[A]): A = as match {
    case Nil | _ :: Nil => throw new NoSuchElementException
    case x :: _ :: Nil  => x
    case _ :: t         => penultimate(t)
  }

}

class P02 extends Sandbox {
  import P02._

  test("should throw an exception on empty list") {
    a[NoSuchElementException] should be thrownBy penultimate(List.empty[Int])
  }

  test("should throw an exception on list size of 1") {
    a[NoSuchElementException] should be thrownBy penultimate(List(13))
  }

  test("should return penultimate element") {
    penultimate(List(1, 1, 2, 3, 5, 8)) shouldEqual 5
  }

}
