package problems

import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p62b]] */
object P62B {}

class P62B extends Sandbox {
  import P55._

  val t = Node(
    'a',
    Node('b', Node('c'), Node('d')),
    Node('e', Node('f'), Node('g'))
  )

  test("collect atLevel") {
    t.collectAtLevel(1) shouldBe "a".toList
    t.collectAtLevel(2) shouldBe "be".toList
    t.collectAtLevel(3) shouldBe "cdfg".toList
  }

}
