package problems

import pprint.log
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p58]] */
object P58 {
  import P55._
  import problems.P55.Tree._

  def mkSymmetricBalanced[A](value: A, n: Int): List[Tree[A]] =
    List(n)
      .filter(isOdd)
      .flatMap(x => mkCompletelyBalanced(value, x))
      .filter(_.isSymmetric)

}

class P58 extends Sandbox {
  import P55._
  import P58._

  test("all symmetric trees") {
    val data = Table(
      inHeader,
      0,
      1,
      2,
      3,
      4,
      5
    )

    forAll(data) { n =>
      val t = mkSymmetricBalanced("a", n)
      log(n -> t)
    }
  }

}
