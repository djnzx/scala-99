package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p12]] */
object P12A {

  def unpackOne[A](a: A, n: Int): List[A] = List.fill(n)(a)

  /** eager, but tail recursive */
  def unpack[A](xs: List[(Int, A)]): List[A] = {

    @tailrec
    def go(xs: List[(Int, A)], acc: List[A]): List[A] = xs match {
      case Nil         => acc
      case (n, c) :: t => go(t, unpackOne(c, n) ::: acc)
    }

    go(xs, Nil) reverse
  }

  def unpackFlatMap[A](xs: List[(Int, A)]): List[A] =
    xs.flatMap { case (cnt, a) => unpackOne(a, cnt) }

}

class P12A extends Sandbox {
  import P12A._

  test("1") {
    val ts = Vector(
      List()                   -> "",
      List((1, 'A'))           -> "A",
      List((2, 'A'))           -> "AA",
      List((2, 'A'), (1, 'B')) -> "AAB",
      List((2, 'A'), (2, 'B')) -> "AABB"
    )

    ts.foreach { case (in, out) =>
      unpack(in) shouldEqual out.toList
    }
  }

}
