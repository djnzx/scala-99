package problems

import scala.annotation.tailrec
import tools.Ex
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p12]] */
object P12B extends Ex {

  import P12A.unpackOne

  /** lazy, recursive, non-stack safe */
  def unpackUnsafeR(xs: List[Any]): List[Char] = xs match {
    case Nil                    => Nil
    case (c: Char) :: t         => c :: unpackUnsafeR(t)
    case (n: Int, c: Char) :: t => unpackOne(c, n) ::: unpackUnsafeR(t)
    case _                      => unexpected
  }

  /** eager, tail-recursive, stack-safe */
  def unpackUnsafeTR(xs: List[Any]): List[Char] = {

    @tailrec
    def go(xs: List[Any], acc: List[Char]): List[Char] = xs match {
      case Nil                    => acc.reverse
      case (c: Char) :: t         => go(t, unpackOne(c, 1) ::: acc)
      case (n: Int, c: Char) :: t => go(t, unpackOne(c, n) ::: acc)
      case _                      => unexpected
    }

    go(xs, Nil)
  }

}

class P12B extends Sandbox {
  import P12B._

  val ts = Vector(
    List()                                  -> "",
    List('A')                               -> "A",
    List((2, 'A'))                          -> "AA",
    List((2, 'A'), 'B')                     -> "AAB",
    List((2, 'A'), (2, 'B'))                -> "AABB",
    List((2, 'A'), (2, 'B'), 'C')           -> "AABBC",
    List((2, 'A'), (2, 'B'), 'C', (3, 'D')) -> "AABBCDDD"
  )

  test("1") {
    ts.foreach { case (in, out) =>
      unpackUnsafeR(in) shouldEqual out.toList
    }
  }

  test("2") {
    ts.foreach { case (in, out) =>
      unpackUnsafeTR(in) shouldEqual out.toList
    }
  }

}
