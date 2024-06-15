package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p11]] */
object P11 {

  def rleUnsafe[A](xs: List[A]): List[Any] = {

    @tailrec
    def go(xs: List[A], cur: Option[(A, Int)], outcome: List[Any]): List[Any] = (xs, cur) match {
      case (Nil, None)                           => outcome                             // first step, empty given
      case (xh :: xt, None)                      => go(xt, Some((xh, 1)), outcome)      // first step, non-empty given
      case (Nil, Some((c, 1)))                   => c :: outcome                        // last step, collected only one
      case (Nil, Some(t))                        => t :: outcome                        // last step, collected >1
      case (xh :: xt, Some((c, cnt))) if xh == c => go(xt, Some((c, cnt + 1)), outcome) // the same letter, keep counting
      case (xh :: xt, Some((c, 1)))              => go(xt, Some((xh, 1)), c :: outcome) // the letter is different, start counting from 1
      case (xh :: xt, Some(t))                   => go(xt, Some((xh, 1)), t :: outcome) // the letter is different, start counting from 1
    }

    go(xs, None, Nil).reverse
  }

  sealed trait Or[A]
  case class Just[A](a: A) extends Or[A]
  case class Group[A](a: A, cnt: Int) extends Or[A]
  object Or {
    def apply[A](a: A): Or[A] = Just(a)
    def apply[A](t: (A, Int)): Or[A] = t match {
      case (a, 1) => Just(a)
      case (a, n) => Group(a, n)
    }
  }

  def rleSafe[A](xs: List[A]): List[Or[A]] = {

    @tailrec
    def go(xs: List[A], cur: Option[(A, Int)], outcome: List[Or[A]]): List[Or[A]] = (xs, cur) match {
      case (Nil, None)                           => outcome                                       // first step, empty given
      case (xh :: xt, None)                      => go(xt, Some((xh, 1)), outcome)                // first step, non-empty given
      case (Nil, Some((c, 1)))                   => Just(c) :: outcome                            // last step, collected only one
      case (Nil, Some((c, n)))                   => Group(c, n) :: outcome                        // last step, collected >1
      case (xh :: xt, Some((c, cnt))) if xh == c => go(xt, Some((c, cnt + 1)), outcome)           // the same letter, keep counting
      case (xh :: xt, Some((c, 1)))              => go(xt, Some((xh, 1)), Just(c) :: outcome)     // the letter is different, start counting from 1
      case (xh :: xt, Some((c, n)))              => go(xt, Some((xh, 1)), Group(c, n) :: outcome) // the letter is different, start counting from 1
    }

    go(xs, None, Nil).reverse
  }

  def rleSafeDsl[A](xs: List[A]): List[Or[A]] = {

    @tailrec
    def go(xs: List[A], cur: Option[(A, Int)], outcome: List[Or[A]]): List[Or[A]] = (xs, cur) match {
      case (Nil, None)                           => Nil                                     // first step, empty given
      case (xh :: xt, None)                      => go(xt, Some(xh -> 1), outcome)          // first step, non-empty given
      case (Nil, Some(t))                        => (Or(t) :: outcome).reverse              // last step
      case (xh :: xt, Some((c, cnt))) if xh == c => go(xt, Some(c -> (cnt + 1)), outcome)   // the same letter, keep counting
      case (xh :: xt, Some(t))                   => go(xt, Some(xh -> 1), Or(t) :: outcome) // the letter is different, collect and start from 1
    }

    go(xs, None, Nil)
  }

}

class P11 extends Sandbox {
  import P11._

  val dataUnsafe = Seq(
    ""     -> List.empty,
    "A"    -> List('A'),
    "AA"   -> List(('A', 2)),
    "AAB"  -> List(('A', 2), 'B'),
    "AABB" -> List(('A', 2), ('B', 2))
  )

  test("unsafe version") {
    for {
      (in, out) <- dataUnsafe
    } rleUnsafe(in.toList) shouldEqual out

  }

  val dataSafe = Seq(
    ""     -> List.empty,
    "A"    -> List(Just('A')),
    "AA"   -> List(Group('A', 2)),
    "AAB"  -> List(Group('A', 2), Just('B')),
    "AABB" -> List(Group('A', 2), Group('B', 2))
  )

  test("safe version") {
    for {
      (in, out) <- dataSafe
    } rleSafe(in.toList) shouldEqual out
  }

  test("safe version (smart constructors)") {
    for {
      (in, out) <- dataSafe
    } rleSafeDsl(in.toList) shouldBe out
  }

}
