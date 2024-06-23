package problems

import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p72]]
  */
object P72 {

  import P70._

  def postOrder[A](t: MTree[A]): List[A] =
    t.children.flatMap(postOrder) ::: List(t.value)

}

class P72 extends Sandbox {

  import P70._
  import P72._

  val t0 = MTree(
    'a',
    List(
      MTree('f', List(MTree('g'))),
      MTree('c'),
      MTree('b', List(MTree('d'), MTree('e')))
    )
  )

  val expected = "gfcdeba".toList

  test("postOrder") {
    postOrder(t0) shouldBe expected
  }

}
