package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p14]] */
object P14 {

  def duplicateR[A](xs: List[A]): List[A] = xs match {
    case Nil    => Nil
    case h :: t => h :: h :: duplicateR(t)
  }

  def duplicateTR[A](xs: List[A]): List[A] = {

    @tailrec
    def go(xs: List[A], outcome: List[A]): List[A] = xs match {
      case Nil    => outcome.reverse
      case h :: t => go(t, h :: h :: outcome)
    }

    go(xs, Nil)
  }
}

class P14 extends Sandbox {

  import P14._

  test("1") {

    val impls = Seq(
      duplicateR[Char] _,
      duplicateTR[Char] _
    )

    val data = Vector(
      ""   -> "",
      "A"  -> "AA",
      "AB" -> "AABB"
    )

    for {
      impl      <- impls
      (in, out) <- data
    } impl(in.toList) shouldBe out.toList

  }
}
