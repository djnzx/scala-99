package problems

import pprint.log
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p57]] */
object P57 {
  import P55._

  def bstFromList[A: Ordering](xs: List[A]): Tree[A] = {

    def go(xs: List[A], t: Tree[A]): Tree[A] = xs match {
      case Nil     => t
      case x :: xs => go(xs, t.add(x))
    }

    go(xs, End)
  }

  def bstFromSeq[A: Ordering](xs: Seq[A]): Tree[A] =
    xs.foldLeft(End: Tree[A])((t, x) => t.add(x))

}

class P57 extends Sandbox {
  import P55._
  import P57._

  test("add base") {
    val data = Table(
      inOutHeader,
      (End, 5)                                       -> Node(5),
      (Node(5), 5)                                   -> Node(5),
      (Node(3), 5)                                   -> Node(3, End, Node(5)),
      (Node(3), 1)                                   -> Node(3, Node(1), End),
      (Node(3, Node(2), Node(10)), 1)                -> Node(3, Node(2, Node(1), End), Node(10)),
      (Node(3, Node(2), Node(10, End, Node(20))), 8) -> Node(3, Node(2), Node(10, Node(8), Node(20)))
    )

    forAll(data) { case ((t, x), out) =>
      t.add(x) shouldBe out
    }
  }

  test("add another") {
    (1 to 5).foldLeft(End: Tree[Int])((t, x) => t.add(x)) shouldBe
      Node(1, End, Node(2, End, Node(3, End, Node(4, End, Node(5, End, End)))))
  }

  test("add yet another") {
    val built = bstFromList(List(4, 2, 1, 3, 15, 10, 20))
    log(built, width = 200)
    built shouldBe Node(4, Node(2, Node(1), Node(3)), Node(15, Node(10), Node(20)))
  }

  test("fromList/fromSeq") {
    val data = Table(
      inHeader,
      List(5, 3, 18, 1, 4, 12, 21),
      List(3, 2, 5, 7, 4)
    )

    forAll(data) { in =>
      bstFromSeq(in) shouldBe bstFromList(in)
    }

  }

  test("is-symmetric") {
    val data = Table(
      inOutHeader,
      bstFromList(List(5, 3, 18, 1, 4, 12, 21)) -> true,
      bstFromList(List(3, 2, 5, 7, 4))          -> false
    )

    forAll(data) { case (in, exp) =>
      in.isSymmetric shouldBe exp
    }
  }

}
