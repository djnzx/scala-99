package problems

import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p63]] */
object P63 {}

class P63 extends Sandbox {
  import P55._

  test("mkLR") {
    Tree.mkLR(1) shouldBe 0 -> 0
    Tree.mkLR(2) shouldBe 1 -> 0
    Tree.mkLR(3) shouldBe 1 -> 1
    Tree.mkLR(4) shouldBe 2 -> 1
    Tree.mkLR(5) shouldBe 3 -> 1
    Tree.mkLR(6) shouldBe 3 -> 2
    Tree.mkLR(7) shouldBe 3 -> 3
    Tree.mkLR(8) shouldBe 4 -> 3
    Tree.mkLR(9) shouldBe 5 -> 3
    Tree.mkLR(10) shouldBe 6 -> 3
    Tree.mkLR(11) shouldBe 7 -> 3
    Tree.mkLR(12) shouldBe 7 -> 4
    Tree.mkLR(13) shouldBe 7 -> 5
    Tree.mkLR(14) shouldBe 7 -> 6
    Tree.mkLR(15) shouldBe 7 -> 7
  }

  test("complete binary tree") {
    val testData = Table(
      inHeader,
      0,
      1,
//      2,
//      3,
//      4,
//      5,
//      6,
//      7,
//      8,
//      9,
//      10,
//      11,
//      12,
//      13,
//      14,
//      15,
    )
    forAll(testData) { n =>
      val t = Tree.mkCompleteBinaryTree("x", n)
      pprint.log(n -> t, showFieldNames = false)
    }
  }

}
