package problems

import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p59]] */
object P59 {}

class P59 extends Sandbox {
  import P55._

  test("all height balanced trees") {
    val data = Table(
      inHeader,
      0,
      1,
      2,
      3,
      4,
      5
    )

    val x = Tree.mkHeightBalanced("a", 4)
    pprint.log(x)

//
//    forAll(data) { n =>
//      val t = Tree.heightBalanced("a", 9)
//      log(n -> t)
//    }
  }

}
