package problems

import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p68]]
  */
object P68 {
  import P55._

  def splitByNeedle[A](inOrder: List[A], preTailHead: A): (List[A], List[A]) =
    inOrder.span(_ != preTailHead) match {
      case (l, r) => l -> r.tail
    }

  def reconstruct[A](preOrder: List[A], inOrder: List[A]): Tree[A] = preOrder match {
    case Nil                => End
    case h :: Nil           => Node(h)
    case preHead :: preTail =>
      val (inL, inR) = splitByNeedle(inOrder, preHead)
      val (preL, preR) = preTail.splitAt(inL.size)
      val l = reconstruct(preL, inL)
      val r = reconstruct(preR, inR)
      Node(preHead, l, r)
  }

}

class P68 extends Sandbox {

  import P55._
  import P64A._
  import P68._

  test("empty") {
    val t0 = End
    val pr = tPreOrder(t0)
    val in = tInOrder(t0)
    reconstruct(pr, in) shouldBe t0
  }

  test("one value") {
    val t0 = Node('a')
    val pr = tPreOrder(t0)
    val in = tInOrder(t0)
    reconstruct(pr, in) shouldBe t0
  }

  test("one full node: 3") {
    val t0 = Node('a', 'b', 'c')
    val pr = tPreOrder(t0)
    val in = tInOrder(t0)
    reconstruct(pr, in) shouldBe t0
  }

  test("one full node - left") {
    val t0 = Node('a', Node('b'), End)
    val pr = tPreOrder(t0)
    val in = tInOrder(t0)
    reconstruct(pr, in) shouldBe t0
  }

  test("one full node - right") {
    val t0 = Node('a', End, Node('c'))
    val pr = tPreOrder(t0)
    val in = tInOrder(t0)
    reconstruct(pr, in) shouldBe t0
  }

  test("left chain") {
    val t0 = Node(
      'a',
      Node(
        'b',
        Node(
          'c',
          Node(
            'd',
            Node('e'),
            End
          ),
          End
        ),
        End,
      ),
      End,
    )
    val pr = tPreOrder(t0)
    val in = tInOrder(t0)
    reconstruct(pr, in) shouldBe t0
  }

  test("right chain") {
    val t0 = Node(
      'a',
      End,
      Node(
        'b',
        End,
        Node(
          'c',
          End,
          Node(
            'd',
            End,
            Node('e'),
          ),
        ),
      ),
    )
    val pr = tPreOrder(t0)
    val in = tInOrder(t0)
    reconstruct(pr, in) shouldBe t0
  }

  test("big") {
    val t0 = Node(
      'a',
      Node(
        'b',
        Node('d', 'h', 'i'),
        Node('e', 'j', 'k'),
      ),
      Node(
        'c',
        Node('f', 'l', 'm'),
        Node('g', 'n', 'o'),
      ),
    )
    val pr = tPreOrder(t0)
    val in = tInOrder(t0)
    reconstruct(pr, in) shouldBe t0
  }

  test("span") {
    val xs = List(1, 2, 3, 4, 5)
    splitByNeedle(xs, 4) shouldBe List(1, 2, 3) -> List(5)
  }

}
