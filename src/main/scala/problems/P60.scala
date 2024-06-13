package problems

import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p60]] */
object P60 {}

class P60 extends Sandbox {
  import P55._

  /*
   -----|-----------------------
     h  | n elements
   -----|-----------------------
     0  | 0..0
     1  | 1..1   ,   1 ≤ n < 2^1
     2  | 2..3   ,   2 ≤ n < 2^2
     3  | 4..7   ,   4 ≤ n < 2^3
     4  | 7..15  ,   7 ≤ n < 2^4
     5  | 12..31 ,  12 ≤ n < 2^5
   -----|-----------------------
   */
  test("min/max HbalNodes per given h") {

    val data = Table(
      "h" -> "min nodes/max nodes",
      0   -> (0, 0),
      1   -> (1, 1),
      2   -> (2, 3),
      3   -> (4, 7),
      4   -> (7, 15),
      5   -> (12, 31),
      6   -> (20, 63),
      7   -> (33, 127),
      8   -> (54, 255)
    )

    forAll(data) { case (h, (minExp, maxExp)) =>
      val min = Tree.minHbalNodes(h)
      val max = Tree.maxHbalNodes(h)
      min shouldBe minExp
      max shouldBe maxExp
    }

  }

  /*

     o          o           o            o               o            o       |
    / \       /   \       /   \        /   \           /   \        /   \     |
   o   o     o     o     o     o     o       o       o       o     o      o   |
      / \     \     \     \   /     /         \     /      /      / \         |
     o   o     o     o     o o     o           o   o     a       o   o        |

   */

  test("all height balanced trees with N Nodes") {
    val data = Table(
      "n" -> "number of tree",
      3   -> 1,
      4   -> 4,
      5   -> 6,
      6   -> 4,
      7   -> 17,
      8   -> 32,
      9   -> 44,
      10  -> 60,
      11  -> 70,
      12  -> 184,
      13  -> 476,
      14  -> 872,
      15  -> 1553
    )

    forAll(data) { (n, cnt) =>
      val t1 = Tree.mkHeightBalancedN("a", n)
//      pprint.log(n -> t1, showFieldNames = false)
      pprint.log(n -> t1.size, showFieldNames = false)
      t1.size shouldBe cnt

      val t2 = Tree.mkHeightBalancedN2("a", n)
//      pprint.log(n -> t2, showFieldNames = false)
      pprint.log(n -> t2.size, showFieldNames = false)
      t2.size shouldBe cnt
    }
  }

  test("min/max HbalHeight for the riven N") {
    val min = Tree.minHbalHeight(5)
    val max = Tree.maxHbalHeight(5)
    pprint.log(min -> max)
  }

}
