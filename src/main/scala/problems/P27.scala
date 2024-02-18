package problems

import pprint.pprintln
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p27]] */
object P27 {
  import P26.{pickN => combinations}

  implicit class ListOps[A](private val as: List[A]) extends AnyVal {
    def --(bs: List[A]): List[A] = as.filterNot(bs.contains)
  }

  def group3[A](as: List[A]) =
    for {
      xs <- combinations(2, as)  // groups of 2
      woA = as -- xs
      ys <- combinations(3, woA) // groups of 3 w.o original ones
      zs = woA -- ys // group of N-2-3
    } yield List(xs, ys, zs)

  //                                        variants
  //                                             groups
  //                                                  group
  def groupN1[A](ns: List[Int], la: List[A]): List[List[List[A]]] =
    ns match {
      case Nil     => List(Nil)
      case n :: ns =>
        require((ns.sum + n) == la.length, "Sum of the list should equal to number of the list items")
        combinations(n, la) // possible groups of N items
          .flatMap { cs: List[A] => // c       - subgroup
            groupN1(ns, la -- cs)   // la -- c - rest of the team, build recursively
              .map(cs :: _) // attach subgroup
          }
    }

  def groupN2[A](ns: List[Int], la: List[A]): List[List[List[A]]] = ns match {
    case Nil     => List(Nil)
    case n :: ns =>
      for {
        c <- combinations(n, la)
        g <- groupN2(ns, la -- c).map(c :: _)
      } yield g
  }

}

class P27 extends Sandbox {
  import P27._

  test("1") {
    val origin = "abcde".toList
    val expected = List(
      List(List('a', 'b'), List('c', 'd', 'e')),
      List(List('a', 'c'), List('b', 'd', 'e')),
      List(List('a', 'd'), List('b', 'c', 'e')),
      List(List('a', 'e'), List('b', 'c', 'd')),
      List(List('b', 'c'), List('a', 'd', 'e')),
      List(List('b', 'd'), List('a', 'c', 'e')),
      List(List('b', 'e'), List('a', 'c', 'd')),
      List(List('c', 'd'), List('a', 'b', 'e')),
      List(List('c', 'e'), List('a', 'b', 'd')),
      List(List('d', 'e'), List('a', 'b', 'c'))
    )
    val groups = List(2, 3)
    val actual1 = groupN1(groups, origin)
    val actual2 = groupN2(groups, origin)

    actual2.foreach(x => pprintln(x))

    actual1 shouldBe expected
    actual2 shouldBe expected
  }

}
