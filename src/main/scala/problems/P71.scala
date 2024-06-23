package problems

import tools.Sandbox

/** https://aperiodic.net/pip/scala/s-99/#p71
  */
object P71 {

  import P70._

  def internalLength(t: MTree[_]): Int = {

    def go(t: MTree[_], level: Int): Int = t match {
      case MTree(_, Nil)      => 0
      case MTree(_, children) =>
        children
          .map(t => level + 1 + go(t, level + 1))
          .sum
    }

    go(t, 0)
  }

}

class P71 extends Sandbox {

  import P70._
  import P71._

  test("internalLength - 0") {
    val t0 = MTree('a')
    internalLength(t0) shouldBe 0
  }

  test("internalLength - 1") {
    val t0 = MTree('a', MTree('b'))
    internalLength(t0) shouldBe 1
  }

  test("internalLength - 2 leafs") {
    val t0 = MTree('a', MTree('b'), MTree('c'))
    internalLength(t0) shouldBe 2
  }

  test("internalLength - 3") {
    val t0 = MTree('a', MTree('b', MTree('c')))
    internalLength(t0) shouldBe 3
  }

  test("internalLength - 9") {
    val t0 = MTree(
      'a',
      List(
        MTree('f', List(MTree('g'))),
        MTree('c'),
        MTree('b', List(MTree('d'), MTree('e')))
      )
    )
    internalLength(t0) shouldBe 9
  }

  test("internalLength - 11") {
    val t0 = MTree(
      'a',
      List(
        MTree('f', List(MTree('g'))),
        MTree('c'),
        MTree('b', List(MTree('d'), MTree('e'), MTree('x')))
      )
    )
    internalLength(t0) shouldBe 11
  }

}
