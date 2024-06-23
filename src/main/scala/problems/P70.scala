package problems

import cats.implicits.catsSyntaxOptionId
import cats.implicits.toFoldableOps
import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p70c]]
  * [[https://aperiodic.net/pip/scala/s-99/p70.svg]]
  */
object P70 {

  case class MTree[+A](value: A, children: List[MTree[A]]) {
    override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
  }

  object MTree {
    def apply[A](value: A): MTree[A] = new MTree(value, List())
    def apply[A](value: A, children: List[MTree[A]]): MTree[A] = new MTree(value, children)
    def apply[A](value: A, children: A*): MTree[A] = new MTree(value, children.map(MTree(_)).toList)
  }

  def countNodes(t: MTree[_]): Int =
    1 + t.children.map(countNodes).sum

  // cats based
  def countNodesV1(t: MTree[_]): Int =
    1 + t.children.foldMap(countNodesV1)

  def countNodesV2(t: MTree[_]): Int =
    t.children.foldLeft(1)(_ + countNodesV2(_))

  // stringify (preorder)
  def stringify(t: MTree[Char]): String =
    t.value.toString + t.children.foldRight("^")((n, acc) => stringify(n) + acc)

  def parsePreorder(raw: String): MTree[Char] = {

    def nodes(idx: Int): (List[MTree[Char]], Int) = node(idx) match {
      case (None, idx)    => Nil -> idx
      case (Some(t), idx) =>
        val (sub, idx2) = nodes(idx)
        (t :: sub) -> idx2
    }

    def node(idx: Int): (Option[MTree[Char]], Int) = raw(idx) match {
      case '^' => None -> (idx + 1)
      case c   =>
        val (children, idx2) = nodes(idx + 1)
        MTree(c, children).some -> idx2
    }

    node(idx = 0)._1.getOrElse(throw new RuntimeException("broken or empty string given"))
  }
}

class P70 extends Sandbox {

  import P70._

  val t0 = MTree(
    'a',
    List(
      MTree('f', List(MTree('g'))),
      MTree('c'),
      MTree('b', List(MTree('d'), MTree('e')))
    )
  )

  val t0s = "afg^^c^bd^e^^^"

  test("count - my solution") {
    countNodes(t0) shouldBe 7
  }

  test("count - proposed solution") {
    countNodesV2(t0) shouldBe 7
  }

  test("stringify") {
    stringify(t0) shouldBe t0s
  }

  test("parsePreorder") {
    parsePreorder(t0s) shouldBe t0
  }

}
