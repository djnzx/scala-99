package problems

import pprint.log
import scala.math.Ordered.orderingToOrdered
import tools.Sandbox

/** Construct completely balanced binary trees
  * for every node: l.size - r.size <=1
  * [[https://aperiodic.net/phil/scala/s-99/#p55]]
  */
object P55 {

  sealed trait Tree[+A] {
    def mirror: Tree[A] = this match {
      case Node(value, l, r) => Node(value, r, l)
      case t @ End           => t
    }
    def isMirrorOf[B](that: Tree[B]): Boolean = (this, that) match {
      case (End, End)                         => true
      case (Node(_, ll, lr), Node(_, rl, rr)) => ll.isMirrorOf(rr) && lr.isMirrorOf(rl)
      case (_, _)                             => false
    }
    def isSymmetric: Boolean = this match {
      case End           => true
      case Node(_, l, r) => l isMirrorOf r
    }
    // BST add note to help compiler due to the fact B in the contravariant position
    def add[B >: A](x: B)(implicit ev: Ordering[B]): Tree[B] = this match {
      case End                            => Node(x)
      case Node(value, l, r) if x < value => Node(value, l.add(x), r)
      case Node(value, l, r) if x > value => Node(value, l, r.add(x))
      case theSame                        => theSame
    }
  }
  case class Node[+A](value: A, l: Tree[A], r: Tree[A]) extends Tree[A]
  case object End extends Tree[Nothing]
  object Node {
    def apply[A](value: A): Node[A] = Node(value, End, End)
  }
  object Tree {

    /** given number, provide all possible combinations of sizes for left & right sub-trees */
    def combsLR(n: Int): List[(Int, Int)] = n match {
      case 0                 => List.empty
      case n if (n & 1) == 0 =>
        val half = n / 2
        List(half -> half)
      case n1                =>
        val half = n1 / 2
        val pair = half -> (n1 - half)
        List(pair, pair.swap)
    }

    def cBalanced[A](value: A, n: Int): List[Tree[A]] = n match {
      case 0 => List(End)
      case 1 => List(Node(value))
      case n =>
        combsLR(n - 1)
          .flatMap { case (l, r) =>
            val ls = cBalanced(value, l)
            val rs = cBalanced(value, r)

            ls.flatMap(lt => rs.map(rt => Node(value, lt, rt)))
          }
    }

    def symmetricBalancedTrees[A](value: A, n: Int) =
      List(n)
        .filter(x => (x & 1) == 1)
        .flatMap(x => cBalanced(value, x))
        .filter(_.isSymmetric)

    def fromSeq[A: Ordering](xs: Seq[A]): Tree[A] =
      xs.foldLeft(End: Tree[A])((t, x) => t.add(x))

    def fromList[A: Ordering](xs: List[A]): Tree[A] = {

      def go(xs: List[A], t: Tree[A]): Tree[A] = xs match {
        case Nil     => t
        case x :: xs => go(xs, t.add(x))
      }

      go(xs, End)
    }
  }
}

class P55 extends Sandbox {
  import P55._

  test("api") {
    val t1 = Node(3)
    log(t1)
    val t2 = Node(5, Node(3), Node(9))
    log(t2)
  }

  test("l/r size combinations") {
    val data = Table(
      inOutHeader,
      0 -> List(),
      1 -> List(0 -> 1, 1 -> 0),
      2 -> List(1 -> 1),
      3 -> List(1 -> 2, 2 -> 1)
    )

    forAll(data) { (in, out) =>
      Tree.combsLR(in) shouldBe out
    }
  }

  test("all tree combinations") {
    val data = Table(
      inOutHeader,
      1 -> List(
        Node(value = 'Z', l = End, r = End)
      ),
      2 -> List(
        Node(value = 'Z', l = End, r = Node(value = 'Z', l = End, r = End)),
        Node(value = 'Z', l = Node(value = 'Z', l = End, r = End), r = End)
      ),
      3 -> List(
        Node(
          value = 'Z',
          l = Node(value = 'Z', l = End, r = End),
          r = Node(value = 'Z', l = End, r = End)
        )
      ),
      4 -> List(
        Node(
          value = 'Z',
          l = Node(value = 'Z', l = End, r = End),
          r = Node(value = 'Z', l = End, r = Node(value = 'Z', l = End, r = End))
        ),
        Node(
          value = 'Z',
          l = Node(value = 'Z', l = End, r = End),
          r = Node(value = 'Z', l = Node(value = 'Z', l = End, r = End), r = End)
        ),
        Node(
          value = 'Z',
          l = Node(value = 'Z', l = End, r = Node(value = 'Z', l = End, r = End)),
          r = Node(value = 'Z', l = End, r = End)
        ),
        Node(
          value = 'Z',
          l = Node(value = 'Z', l = Node(value = 'Z', l = End, r = End), r = End),
          r = Node(value = 'Z', l = End, r = End)
        )
      )
    )

    forAll(data) { (in, out) =>
      val actual = Tree.cBalanced('Z', in)
//      log(actual)
      actual shouldBe out
    }
  }

}
