package problems

import scala.annotation.tailrec
import tools.Sandbox

/** https://aperiodic.net/phil/scala/s-99/#p03 */
object P03 {

  @tailrec
  def nth[A](n: Int, as: List[A]): A = (n, as) match {
    case (_, Nil)    => throw new NoSuchElementException
    case (0, h :: _) => h
    case (n, _ :: t) => nth(n - 1, t)
  }

}

class P03 extends Sandbox {
  import P03._

  test("should throw an exception on empty list") {
    a[NoSuchElementException] should be thrownBy nth(0, List.empty[Int])
  }

  test("should throw an exception on N > list.length") {
    a[NoSuchElementException] should be thrownBy nth(5, List(1, 2, 3, 4, 5))
  }

  test("should provide an K-th element of the list N <= list length") {
    nth(3, List(10, 20, 30, 40)) shouldEqual 40
    nth(2, List(10, 20, 30, 40)) shouldEqual 30
  }
}
