package problems

import cats.implicits.catsSyntaxOptionId
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
  }
  object Tree {

    def isEven(x: Int): Boolean = (x & 1) == 0
    def isOdd: Int => Boolean = isEven

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

    /** 58. is symmetric is a harder limitation */
    def mkSymmetricBalanced[A](value: A, n: Int): List[Tree[A]] =
      List(n)
        .filter(isOdd)
        .flatMap(x => mkCompletelyBalanced(value, x))
        .filter(_.isSymmetric)

    /** 59 */
    def mkHeightBalanced[A](value: A, h: Int): LazyList[Tree[A]] = h match {
      case 0 => LazyList(End)
      case 1 => LazyList(Node(value))
      case h =>
        val full = mkHeightBalanced(value, h - 1)
        val short = mkHeightBalanced(value, h - 2)
        val tt1 = full.flatMap(lt => full.map(rt => Node(value, lt, rt)))
        val tt2 = full.flatMap { ft =>
          short.flatMap { st =>
            LazyList(Node(value, ft, st), Node(value, st, ft))
          }
        }
        tt1 ++ tt2
    }

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

    def fromSeq[A: Ordering](xs: Seq[A]): Tree[A] =
      xs.foldLeft(End: Tree[A])((t, x) => t.add(x))

    /** 57. BST */
    def fromList[A: Ordering](xs: List[A]): Tree[A] = {

      def go(xs: List[A], t: Tree[A]): Tree[A] = xs match {
        case Nil     => t
        case x :: xs => go(xs, t.add(x))
      }

      go(xs, End)
    }

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

    /** 63. */
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
