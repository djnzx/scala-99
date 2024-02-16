package problems

import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p22]] */
object P22 {

  /** built in */
  def range(mn: Int, mx: Int): List[Int] = (mn to mx).toList

  /** built in, Lazy */
  def rangeLazy(mn: Int, mx: Int): List[Int] = LazyList.from(mn).takeWhile(_ <= mx).toList

  /** classic recursive */
  def rangeR(mn: Int, mx: Int): List[Int] =
    if (mn > mx) Nil
    else mn :: rangeR(mn + 1, mx)

  def unfold[S, A](s0: S)(f: S => Option[(S, A)]): List[A] =
    f(s0) match {
      case Some((s1, v)) => v :: unfold(s1)(f)
      case None          => Nil
    }

  /** recursive, but generalized */
  def rangeUnfold(mn: Int, mx: Int): List[Int] =
    unfold(mn) { s =>
      if (s > mx) None
      else Some((s + 1, s))
    }

  /** classic tail-recursive */
  def rangeTr(mn: Int, mx: Int): List[Int] = {

    def go(mn: Int, acc: List[Int]): List[Int] =
      if (mn > mx) acc reverse
      else go(mn + 1, mn :: acc)

    go(mn, Nil)
  }
}

class P22 extends Sandbox {
  import P22._

  test("1") {
    val mn = 4
    val mx = 9
    val impls = Seq(
      range _,
      rangeLazy _,
      rangeR _,
      rangeTr _,
      rangeUnfold _,
    )
    val expected = (mn to mx).toList

    impls.foreach(impl => impl(mn, mx) shouldBe expected)
  }
}
