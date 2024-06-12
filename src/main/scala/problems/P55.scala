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
    def size: Int = this match {
      case End           => 0
      case Node(_, l, r) => 1 + l.size + r.size
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

    /** 59.B. given N (Nodes count) is a bit relaxed than completely balanced */
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

    /** 59.A. */
    // TODO: it hangs h > 5, memoize!
    def mkHeightBalanced[A](value: A, h: Int): LazyList[Tree[A]] = {

      def mk(h: Int, cache0: Map[Int, LazyList[Tree[A]]]): (LazyList[Tree[A]], Map[Int, LazyList[Tree[A]]]) =
        h match {
          case 0 =>
            cache0.get(h) match {
              case Some(xx) => xx -> cache0
              case None     =>
                val xx = LazyList(End)
                xx -> (cache0 ++ Map(h -> xx))
            }
          case 1 =>
            cache0.get(h) match {
              case Some(xx) => xx -> cache0
              case None     =>
                val xx = LazyList(Node(value))
                xx -> (cache0 ++ Map(h -> xx))
            }
          case _ =>
            val (h1ts, cache1p) = cache0.get(h - 1) match {
              case Some(xx) => xx -> cache0
              case None     =>
                val (xx, cacheA) = mk(h - 1, cache0)
                xx -> (cache0 ++ cacheA)
            }
            pprint.log((h, h - 1, cache1p))
            val cache1 = cache0 ++ cache1p

            val (h2ts, cache2p) = cache1.get(h - 2) match {
              case Some(xx) => xx -> cache1
              case None     =>
                val (xx, cacheA) = mk(h - 2, cache1)
                xx -> (cache1 ++ cacheA)
            }
            pprint.log((h, h - 2, cache2p))

            val tt = h1ts.flatMap(lt => h1ts.map(rt => Node(value, lt, rt))) ++
              h1ts.flatMap(lt => h2ts.map(rt => Node(value, lt, rt))) ++
              h2ts.flatMap(lt => h1ts.map(rt => Node(value, lt, rt)))

            val cache3 = Map(h -> tt)
            tt -> (cache1 ++ cache2p ++ cache3)
        }

      mk(h, Map.empty)._1
    }

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
