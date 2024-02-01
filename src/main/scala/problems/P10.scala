package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p10]] */
object P10 {

  def pack[A](xs: List[A]): List[(A, Int)] = {

    @tailrec
    def go(xs: List[A], buf: Option[(A, Int)], outcome: List[(A, Int)]): List[(A, Int)] = (xs, buf) match {
      case (Nil, None)                       => outcome                             // first, empty given
      case (Nil, Some(t))                    => (t :: outcome).reverse              // last, just add what we have in buffer
      case (x :: xs, None)                   => go(xs, Some(x -> 1), outcome)       // first step
      case (x :: xs, Some((a, n))) if x == a => go(xs, Some(a -> (n + 1)), outcome) // the same letter, keep counting
      case (x :: xs, Some(t)) /* x != a */   => go(xs, Some(x -> 1), t :: outcome)  // the letter is different, start counting from 1
    }

    go(xs, None, Nil)
  }

}

class P10 extends Sandbox {
  import P10._

  test("1") {
    val data = Vector(
      ""          -> List(),
      "A"         -> List('A' -> 1),
      "AA"        -> List('A' -> 2),
      "AB"        -> List('A' -> 1, 'B' -> 1),
      "ABB"       -> List('A' -> 1, 'B' -> 2),
      "AAB"       -> List('A' -> 2, 'B' -> 1),
      "AABB"      -> List('A' -> 2, 'B' -> 2),
      "ABBC"      -> List('A' -> 1, 'B' -> 2, 'C' -> 1),
      "AABBA"     -> List('A' -> 2, 'B' -> 2, 'A' -> 1),
      "ABBCDDD"   -> List('A' -> 1, 'B' -> 2, 'C' -> 1, 'D' -> 3),
      "ABBCCCDDE" -> List('A' -> 1, 'B' -> 2, 'C' -> 3, 'D' -> 2, 'E' -> 1)
    )

    for {
      (in, out) <- data
    } pack(in.toList) shouldBe out

  }

}
