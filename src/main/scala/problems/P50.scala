package problems

import cats.implicits.toFunctorOps
import pprint.log
import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered
import tools.Ex.neverByDesign
import tools.Sandbox

/** [[https://engineering.purdue.edu/ece264/17au/hw/HW13?alt=huffman#:~:text=Huffman%27s%20algorithm%20assumes%20that%20we,roots%20of%20the%20two%20trees.]]
  * [[https://aperiodic.net/phil/scala/s-99/#p50]]
  */
object P50 {

  sealed trait Tree[A] {
    def weight: Int
  }
  case class Leaf[A](a: A, weight: Int) extends Tree[A]
  case class Node[A](l: Tree[A], r: Tree[A], weight: Int) extends Tree[A]
  object Tree {
    def leaf[A](a: A, weight: Int): Tree[A] = Leaf(a, weight)
    def node[A](l: Tree[A], r: Tree[A]): Tree[A] = Node(l, r, l.weight + r.weight)
  }
  def oTree[A]: Ordering[Tree[A]] = Ordering.by(_.weight)

  def mkTreeM[A](pq: mutable.PriorityQueue[Tree[A]]): Tree[A] = pq.size match {
    case 0 => neverByDesign
    case 1 => pq.dequeue()
    case _ =>
      val t1 = pq.dequeue()
      val t2 = pq.dequeue()
      val t = Tree.node(t1, t2)
      pq.enqueue(t)
      mkTreeM(pq)
  }

  def dequeueOneSmallest[A](l1: List[Tree[A]], l2: List[Tree[A]])(implicit ev: Ordering[Tree[A]]): (Tree[A], List[Tree[A]], List[Tree[A]]) = (l1, l2) match {
    case (Nil, Nil)                 => neverByDesign
    case (a :: as, Nil)             => (a, as, Nil)
    case (Nil, b :: bs)             => (b, Nil, bs)
    case (a :: as, b :: _) if a < b => (a, as, l2)
    case (_, b :: bs) /* a >= b */  => (b, l1, bs)
  }

  def mkTreeI[A](l1: List[Tree[A]], l2: List[Tree[A]])(implicit ev: Ordering[Tree[A]]): Tree[A] = (l1, l2) match {
    case (Nil, Nil)     => neverByDesign
    case (List(t), Nil) => t
    case (Nil, List(t)) => t
    case (l1, l2)       =>
      val (t1, l1a, l2a) = dequeueOneSmallest(l1, l2)
      val (t2, l1b, l2b) = dequeueOneSmallest(l1a, l2a)
      val t = Tree.node(t1, t2)
      val l2c = l2b :+ t
      mkTreeI(l1b, l2c)
  }

  def treeToCodes[A](tree: Tree[A]): Map[A, String] = {
    def go(t: Tree[A], path: List[Int], state: Map[A, String]): Map[A, String] = t match {
      case Leaf(a, _)    => state + (a -> path.reverse.mkString)
      case Node(l, r, _) =>
        val sl = go(l, 0 :: path, state)
        val sr = go(r, 1 :: path, state)
        state ++ sl ++ sr
    }
    go(tree, Nil, Map.empty)
  }

  def toLeafs[A](xs: List[(A, Int)]): List[Tree[A]] = xs.map { case (a, w) => Tree.leaf(a, w) }

  /** implementation based on the mutable PriorityQueue */
  def huffmanM[A](xs: List[(A, Int)], trace: Boolean = false): (Map[A, String], Tree[A]) = {
    val ts = toLeafs(xs)
    val tsQ = mutable.PriorityQueue.from(ts)(oTree[A].reverse)
    val t = mkTreeM(tsQ)
    if (trace) log(t)
    treeToCodes(t) -> t
  }

  /** implementation based on two immutable Lists */
  def huffmanI[A](xs: List[(A, Int)], trace: Boolean = false): Map[A, String] = {
    val ts = toLeafs(xs)
    implicit def ot = oTree[A]
    val t = mkTreeI(ts.sorted, List.empty)
    if (trace) log(t)
    treeToCodes(t)
  }

}

class P50 extends Sandbox {

  import P50._
  import pprint.log

  test("huffman") {
    val in = List("a" -> 45, "b" -> 13, "c" -> 12, "d" -> 16, "e" -> 9, "f" -> 5)
    val out = List("a" -> "0", "b" -> "101", "c" -> "100", "d" -> "111", "e" -> "1101", "f" -> "1100")

    huffmanM(in, trace = true)._1.toSet shouldBe out.toSet
    huffmanI(in).toSet shouldBe out.toSet
  }

  def toBin(width: Int)(x: Int): String =
    (0 until width)
      .foldLeft(List.empty[Char]) { (s, i) =>
        val bit = ((x >> i) & 1) + '0'
        bit.toChar :: s
      }
      .mkString

  test("toBin") {
    val data = Table(
      inOutHeader,
      (0, 4)   -> "0000",
      (5, 4)   -> "0101",
      (15, 4)  -> "1111",
      (1, 8)   -> "00000001",
      (255, 8) -> "11111111"
    )

    forAll(data) { case ((n, width), out) =>
      toBin(width)(n) shouldBe out
    }
  }

  def log2(x: Int): Int = {
    def go(n: Int): Int = n match {
      case n if (1 << n) >= x => n
      case n                  => go(n + 1)
    }
    go(0)
  }

  test("log2") {
    val data = Table(
      inOutHeader,
      1  -> 0,
      2  -> 1,
      4  -> 2,
      8  -> 3,
      10 -> 4,
      16 -> 4,
      17 -> 5
    )

    forAll(data) { (in, out) =>
      log2(in) shouldBe out
    }
  }

  def toFreq[A](xs: List[A]): Map[A, Int] = xs.groupMapReduce(identity)(_ => 1)(_ + _)

  def toCodes[A](xs: List[A]): Map[A, Int] = toFreq(xs).keys.zipWithIndex.toMap

  def decodeH[A](encoded: String, dict: Tree[A]): Seq[A] = {

    def next(index: Int): Option[(A, Int)] =
      Option.when(index < encoded.length) {

        def go(idx: Int, tree: Tree[A]): (A, Int) = tree match {
          case Leaf(a, _)    => a -> idx
          case Node(l, r, _) =>
            encoded(idx) match {
              case '0' => go(idx + 1, l)
              case '1' => go(idx + 1, r)
              case _   => neverByDesign
            }
        }

        go(index, dict)
      }

    def go(index: Int, outcome: List[A]): List[A] =
      next(index) match {
        case None           => outcome.reverse
        case Some((a, idx)) => go(idx, a :: outcome)
      }

    go(0, List.empty)
  }

  test("optimisation playground") {
    val input = "I'm sure, programming must be easy and interesting"
    log(input)
    log(input.length)
    val encoded1 = input.map(c => toBin(8)(c)).mkString
    log(encoded1)
    log(encoded1.length) // 400

    val fr = toFreq(input.toList)
    log(fr)
    log(fr.size)
    log(log2(fr.size))
    log(input.length * log2(fr.size))

    val codes = {
      val m = toCodes(input.toList)
      val size = log2(m.size)
      m.fmap(toBin(size))
    }
    log(codes)

    // ENCODE based on the DISTINCT values
    val encoded2 = input.map(c => codes(c)).mkString
    log(encoded2)
    log(encoded2.length) // 250
    // DECODE based on the DISTINCT values
    val codesRev: Map[String, Char] = codes.map { case (k, v) => (v, k) }
    val decoded = encoded2.grouped(5).map(c => codesRev(c)).mkString
    log(decoded)

    val (hMap, hTree) = huffmanM(fr.toList)
    log(hMap)
    log(hTree)

    // ENCODE based on the HUFFMAN values
    val encoded3 = input.map(c => hMap(c)).mkString
    log(encoded3)
    log(encoded3.length) // 200
    // DECODE based on the HUFFMAN values
    val decoded3 = decodeH(encoded3, hTree).mkString
    log(decoded3)
  }

}
