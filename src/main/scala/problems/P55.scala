package problems

import cats.implicits.catsSyntaxTuple2Semigroupal
import pprint.log
import scala.math.Ordered.orderingToOrdered
import tools.Sandbox

/** Construct completely balanced binary trees
  * for every node: l.size - r.size <=1
  * [[https://aperiodic.net/phil/scala/s-99/#p55]]
  */
object P55 {

  sealed trait Tree[+A] {

    /** 61 */
    def size: Int = this match {
      case End           => 0
      case Node(_, l, r) => 1 + l.size + r.size
    }
    def leafCount: Int = this match {
      case End           => 0
      case Node(_, l, r) => l.leafCount + r.leafCount
    }

    /** 61A */
    def collectAll: List[A] = this match {
      case End               => List.empty
      case Node(value, l, r) => value :: l.collectAll ::: r.collectAll
    }

    /** leaves only */
    def collectLeaves: List[A] = this match {
      case End                   => List.empty
      case Node(value, End, End) => List(value)
      case Node(_, l, r)         => l.collectLeaves ::: r.collectLeaves
    }

    /** 62A - non leaves only */
    def collectInternal: List[A] = this match {
      case End               => List.empty
      case Node(_, End, End) => List.empty
      case Node(value, l, r) => value :: l.collectInternal ::: r.collectInternal
    }

    /** 62B */
    def collectAtLevel(n: Int): List[A] = this match {
      case End                         => List.empty
      // collect
      case Node(value, _, _) if n == 1 => List(value)
      // dive deeper
      case Node(_, l, r)               => l.collectAtLevel(n - 1) ::: r.collectAtLevel(n - 1)
    }

    def mirror: Tree[A] = this match {
      case Node(value, l, r) => Node(value, r, l)
      case t @ End           => t
    }
    def isMirrorOf[B](that: Tree[B]): Boolean = (this, that) match {
      case (End, End)                         => true
      case (Node(_, ll, lr), Node(_, rl, rr)) => ll.isMirrorOf(rr) && lr.isMirrorOf(rl)
      case (_, _)                             => false
    }

    /** 56 */
    def isSymmetric: Boolean = this match {
      case End           => true
      case Node(_, l, r) => l isMirrorOf r
    }
    def isBalanced(lh: Int, rh: Int): Boolean = math.abs(lh - rh) <= 1
    // None - unbalanced
    // Some(x) - balanced with max height = x
    def isHeightBalanced: Option[Int] = this match {
      case End           => Some(0)
      case Node(_, l, r) =>
        (l.isHeightBalanced, r.isHeightBalanced).tupled
          .filter { case (lh, rh) => isBalanced(lh, rh) }
          .map { case (lh, rh) => lh max rh }
          .map(_ + 1)
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
    def apply[A](value: A, l: A, r: A): Node[A] = Node(value, Node(l), Node(r))
  }
  object Tree {

    def isEven(x: Int): Boolean = (x & 1) == 0
    def isOdd(x: Int): Boolean = !isEven(x)

    object IsOdd {
      def unapply(x: Int): Option[Int] = Some(x).filter(isOdd)
    }
    object IsEven {
      def unapply(x: Int): Option[Int] = Some(x).filter(isEven)
    }

    /** given number, provide all possible combinations of sizes for left & right sub-trees with size diff<=1 */
    def combsLR1(n: Int): List[(Int, Int)] = n match {
      case 0         => List.empty
      case IsEven(n) =>
        val half = n / 2
        List(half -> half)
      case n1        => // odd
        val half = n1 / 2
        val pair = half -> (n1 - half)
        List(pair, pair.swap)
    }

    /** 55. completely balanced all possible trees: n nodes diff <= 1 */
    def mkCompletelyBalanced[A](value: A, n: Int): List[Tree[A]] = n match {
      case 0 => List(End)
      case 1 => List(Node(value))
      case n =>
        combsLR1(n - 1)
          .flatMap { case (l, r) =>
            val ls = mkCompletelyBalanced(value, l)
            val rs = mkCompletelyBalanced(value, r)

            ls.flatMap(lt => rs.map(rt => Node(value, lt, rt)))
          }
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
      Tree.combsLR1(in) shouldBe out
    }
  }

  test("all tree combinations") {
    val data = Table(
      inOutHeader,
      1 -> List(
        Node('Z', End, End)
      ),
      2 -> List(
        Node('Z', End, Node('Z', End, End)),
        Node('Z', Node('Z', End, End), End)
      ),
      3 -> List(
        Node('Z', Node('Z', End, End), Node('Z', End, End))
      ),
      4 -> List(
        Node('Z', Node('Z', End, End), Node('Z', End, Node('Z', End, End))),
        Node('Z', Node('Z', End, End), Node('Z', Node('Z', End, End), End)),
        Node('Z', Node('Z', End, Node('Z', End, End)), Node('Z', End, End)),
        Node('Z', Node('Z', Node('Z', End, End), End), Node('Z', End, End))
      )
    )

    forAll(data) { (in, out) =>
      val actual = Tree.mkCompletelyBalanced('Z', in)
//      pprint.log(actual, showFieldNames = false)
      actual shouldBe out
    }
  }

}
