package problems

import pprint.log
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p58]] */
object P58 {}

class P58 extends Sandbox {
  import P55._

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
      val t = Tree.symmetricBalancedTrees("a", n)
      log(n -> t)
    }
  }

}
