package problems

import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p59]] */
object P59 {}

class P59 extends Sandbox {
  import P55._

  test("all height balanced trees of height") {
    val data = Table(
      inHeader,
//      0, // 1
//      1, // 1
//      2, // N(h-1)×N(h-2) + N(h-2)×N(h-1) + N(h-1)×N(h-1) = 1 + 1 + 1 = 3
      3 // 3×3 + 3×1 + 3×1 = 15
//      4, // 15*15 + 15*3+15*3 = 315
//      5, // 315*315 + 315*15*2 = 108675
//      6, // 108675*108675 + 108675*315*2 = 11'878'720'875
    )

    forAll(data) { h =>
      val ts = Tree.mkHeightBalanced("a", h)
      pprint.log(h -> ts, showFieldNames = false)
      val sizes = ts.map(_.size).distinct.sorted
      pprint.log((h, ts.size, s"${sizes.head}...${sizes.last}"))

    }
  }

}
