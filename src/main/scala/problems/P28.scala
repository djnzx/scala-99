package problems

import org.scalatest.matchers.should.Matchers
import scala.math.Ordered.orderingToOrdered
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p28]] */
object P28 {

  /** fold primitive */
  def fold[A, Z](as: List[A])(z: Z)(f: (Z, A) => Z): Z = {

    def go(as: List[A], z: Z): Z = as match {
      case Nil     => z
      case a :: as => go(as, f(z, a))
    }

    go(as, z)
  }

  /** functional recursive quick sort
    * it's not stable, so it's hard to write assertion for equal elements
    * in this case list of the same length
    */
  def qsort[A](xs: List[A])(implicit oa: Ordering[A]): List[A] = xs match {
    case Nil      => Nil // EMPTY -> already sorted
    case _ :: Nil => xs  // ONE element -> already sorted
    case x :: _   =>
      // that picks xs < x, xs == x, xs > x - in one iteration
      val (lt, eq, gt) = fold(xs)((List.empty[A], List.empty[A], List.empty[A])) {
        case ((lt, eq, gt), a) if a < x => (a :: lt, eq, gt)
        case ((lt, eq, gt), a) if a > x => (lt, eq, a :: gt)
        case ((lt, eq, gt), a)          => (lt, a :: eq, gt)
      }

      qsort(lt) ++ eq ++ qsort(gt)
  }

  def lsortFreq[A, B](xs: List[A])(implicit ev: A => List[B]): List[A] = {
    implicit val sort: Ordering[A] = Ordering.by { x: A => ev(x).length }
    qsort(xs)
  }

}

class P28 extends Sandbox with Matchers {

  import P28._

  val in = List(
    "abcd",
    "ab",
    "z",
    "12345",
    "jkl"
  ).map(_.toList)

  val out = List(
    "z",
    "ab",
    "jkl",
    "abcd",
    "12345"
  ).map(_.toList)

  test("1") {
    qsort(in)(Ordering.by(_.length)) shouldBe out
  }

  test("2") {
    lsortFreq(in) shouldBe out
  }

}
