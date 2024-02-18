package problems

import pprint.log
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p56]] */
object P56 {}

class P56 extends Sandbox {

  import P55._

  test("mirror") {
    val data = Table(
      inOutHeader,
      End                       -> End,
      Node(5)                   -> Node(5),
      Node(6, Node(1), End)     -> Node(6, End, Node(1)),
      Node(6, Node(1), Node(2)) -> Node(6, Node(2), Node(1))
    )

    forAll(data) { (in, out) =>
      val mi = in.mirror
      log(in)
      log(mi)
      mi shouldBe out
    }
  }

  test("is-mirror") {
    val data = Table(
      inOutHeader,
      (End                        -> End)                       -> true,
      (End                        -> Node(5))                   -> false,
      (Node(5)                    -> Node(5))                   -> true,
      (Node(5)                    -> Node(6))                   -> true,
      (Node(6, Node(1), End)      -> Node(6, End, Node(1)))     -> true,
      (Node(6, Node(1), Node(21)) -> Node(6, Node(2), Node(1))) -> true
    )

    forAll(data) { case ((l, r), out) =>
      l.isMirrorOf(r) shouldBe out
    }
  }

  test("is-symmetric") {
    val data = Table(
      inOutHeader,
      End                        -> true,
      Node(5)                    -> true,
      Node(6, Node(1), Node(21)) -> true,
      Node(6, Node(1), End)      -> false
    )

    forAll(data) { case (in, exp) =>
//      log(in)
//      log(in.mirror)
      in.isSymmetric shouldBe exp
    }
  }

}
