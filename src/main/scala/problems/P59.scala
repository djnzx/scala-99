package problems

import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p59]] */
object P59 {}

class P59 extends Sandbox {
  import P55._

  /*

     o          o           o            o               o            o       |
    / \       /   \       /   \        /   \           /   \        /   \     |
   o   o     o     o     o     o     o       o       o       o     o      o   |
      / \     \     \     \   /     /         \     /      /      / \         |
     o   o     o     o     o o     o           o   o     a       o   o        |

   */
  test("all height balanced trees with 5 Nodes") {
    val data = Table(
      inHeader,
      5
    )

    forAll(data) { n =>
      val t = Tree.mkHeightBalancedN("a", n)
      pprint.log(n -> t, showFieldNames = false)
    }
  }

  /*
   h  | n elements
 -----|------------------
   0  | 0
   1  | 1..1     n < 2^1,    1 ≤ n < 2
   2  | 2..3     n < 2^2,    2 ≤ n < 4
   3  | 4..7     n < 2^3,    4 ≤ n < 8
   4  | 7..15    n < 2^4,    7 ≤ n < 16
   5  | 12..31   n < 2^5,   12 ≤ n < 32

   */
  test("all height balanced trees of height") {
    val data = Table(
      inHeader,
//      0, // 1
//      1, // 1
//      2, // N(h-1)×N(h-2) + N(h-2)×N(h-1) + N(h-1)×N(h-1) = 1 + 1 + 1 = 3
//      3, // 3×3 + 3 + 3 = 15
//      4,
//      5,
      6,
//      7,
    )

    forAll(data) { h =>
      val ts = Tree.mkHeightBalanced("a", h)
      pprint.log(h -> ts, showFieldNames = false, width = 1000)

      val sizes = ts.map(_.size).distinct.sorted
      pprint.log(h -> (sizes.head, sizes.last))

    }
  }

}
