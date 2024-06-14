package problems

import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p63]] */
object P63 {
  import P55._

  /*
  12 means
                o
             /    \
           /        \
         /            \
        o              o
      /   \          /   \
     o     o        o     o
   /  \   /  \    /
  o    o o    o  o
   */
  def split(n: Int, w: Int, l: Int, r: Int): (Int, Int) =
    if (n == 0) l -> r
    else if (n > w * 2) split(n - w * 2, w << 1, l + w, r + w)
    else if (n > w) (l + w, r + (n - w))
    else (l + n, r)

  /** sizes for the left and right subtrees */
  def mkLR(n: Int): (Int, Int) = n match {
    case 0 => ???
    case n => split(n - 1, 1, 0, 0)
  }

  def mkCompleteBinaryTree[A](value: A, n: Int): Tree[A] = n match {
    case 0 => End
    case n =>
      val (lc, rc) = mkLR(n)
      Node(
        value,
        mkCompleteBinaryTree(value, lc),
        mkCompleteBinaryTree(value, rc)
      )
  }

  def mkCompleteBinaryTree2[A](value: A, n: Int): Tree[A] = {

    def mk(addr: Int): Tree[A] =
      if (addr > n) End
      else Node(value, mk(2 * addr), mk(2 * addr + 1))

    mk(1)
  }

}

class P63 extends Sandbox {
  import P63._

  test("mkLR") {
    mkLR(1) shouldBe 0  -> 0
    mkLR(2) shouldBe 1  -> 0
    mkLR(3) shouldBe 1  -> 1
    mkLR(4) shouldBe 2  -> 1
    mkLR(5) shouldBe 3  -> 1
    mkLR(6) shouldBe 3  -> 2
    mkLR(7) shouldBe 3  -> 3
    mkLR(8) shouldBe 4  -> 3
    mkLR(9) shouldBe 5  -> 3
    mkLR(10) shouldBe 6 -> 3
    mkLR(11) shouldBe 7 -> 3
    mkLR(12) shouldBe 7 -> 4
    mkLR(13) shouldBe 7 -> 5
    mkLR(14) shouldBe 7 -> 6
    mkLR(15) shouldBe 7 -> 7
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
      val t = mkCompleteBinaryTree("x", n)
      pprint.log(n -> t, showFieldNames = false)
    }
  }

}
