package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p15]] */
object P15 {

  def timesN[A](n: Int, a: A): List[A] = List.fill(n)(a)

  def prependN[A](n: Int, a: A, tail: List[A]): List[A] = n match {
    case 0 => tail
    case n => prependN(n - 1, a, a :: tail)
  }

  def duplicateNR[A](n: Int, xs: List[A]): List[A] = xs match {
    case Nil    => Nil
    case h :: t => timesN(n, h) ++ duplicateNR(n, t)
  }

  def duplicateNTR[A](n: Int, xs: List[A]): List[A] = {

    @tailrec
    def go(xs: List[A], outcome: List[A]): List[A] = xs match {
      case Nil    => outcome.reverse
      case h :: t => go(t, prependN(n, h, outcome))
    }

    go(xs, Nil)
  }
}

class P15 extends Sandbox {

  import P15._

  test("prependN") {
    prependN(2, 'a', "bcdef".toList) shouldBe "aabcdef".toList
  }

  test("impls") {
    val impls = Seq(
      duplicateNR[Char] _,
      duplicateNTR[Char] _
    )

    val data = Seq(
      (1, "")   -> "",
      (1, "A")  -> "A",
      (1, "AB") -> "AB",
      (2, "")   -> "",
      (2, "A")  -> "AA",
      (2, "AB") -> "AABB",
      (3, "")   -> "",
      (3, "A")  -> "AAA",
      (3, "AB") -> "AAABBB"
    )

    for {
      impl           <- impls
      ((n, in), out) <- data
    } impl(n, in.toList) shouldEqual out.toList

  }
}
