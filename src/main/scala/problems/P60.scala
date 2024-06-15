package problems

import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p60]] */
object P60 {
  import P55._
  import P59._

  /** 60 brute-force
    * for N=5
    * we generate 42 trees
    * and filter out 36 of them
    */
  def mkHeightBalancedN[A](value: A, n: Int): LazyList[Tree[A]] = n match {
    case 0 => LazyList(End)
    case 1 => LazyList(Node(value))
    case n =>
      LazyList
        .from(0 to (n - 1)) // left sizes
        .flatMap { l =>
          val r = n - 1 - l           // right sizes
          mkHeightBalancedN(value, l) // all left sub-trees
            .flatMap { lt =>
              mkHeightBalancedN(value, r)       // all right sub trees
                .map(rt => Node(value, lt, rt)) // combine
                .filter(_.isHeightBalanced.isDefined) // check if balanced
            }
        }
  }

  def minHbalNodes(h: Int): Int = h match {
    case 0 => 0
    case 1 => 1
    case n => 1 + minHbalNodes(n - 1) + minHbalNodes(n - 2)
  }
  def maxHbalNodes(h: Int): Int = h match {
    case 0 => 0
    case _ => (2 << (h - 1)) - 1
  }

  def minHbalHeight(n: Int): Int = n match {
    case 0 => 0
    case n => 1 + minHbalHeight(n / 2)
  }
  def maxHbalHeight(n: Int): Int =
    LazyList.from(1).takeWhile(x => minHbalNodes(x) <= n).last

  /** 60 smart, based on the calculation
    * for N=5
    * we generate 15 trees
    * and filter out 9 of them
    */
  def mkHeightBalancedN2[A](value: A, n: Int): LazyList[Tree[A]] =
    LazyList
      .from(minHbalHeight(n) to maxHbalHeight(n))
      .flatMap(h => mkHeightBalanced(value, h))
      .filter(_.size == n)

}

class P60 extends Sandbox {
  import P60._

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
      val min = minHbalNodes(h)
      val max = maxHbalNodes(h)
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
      val t1 = mkHeightBalancedN("a", n)
//      pprint.log(n -> t1, showFieldNames = false)
      pprint.log(n -> t1.size, showFieldNames = false)
      t1.size shouldBe cnt

      val t2 = mkHeightBalancedN2("a", n)
//      pprint.log(n -> t2, showFieldNames = false)
      pprint.log(n -> t2.size, showFieldNames = false)
      t2.size shouldBe cnt
    }
  }

  test("min/max HbalHeight for the riven N") {
    val min = minHbalHeight(5)
    val max = maxHbalHeight(5)
    pprint.log(min -> max)
  }

}
