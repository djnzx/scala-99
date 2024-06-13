package problems

import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p61a]]
  *
  * [[https://aperiodic.net/pip/scala/s-99/#p62]]
  */
object P61 {}

class P61 extends Sandbox {
  import P55._

  val tl = Node('b')
  val tr = Node('c', Node('d'), Node('e'))
  val t = Node('a', tl, tr)

  test("collect all") {
    val ls = t.collectAll
    pprint.log(ls)
    ls shouldBe List('a', 'b', 'c', 'd', 'e')
  }

  test("collect leaves") {
    val ls = t.collectLeaves
    pprint.log(ls)
    ls shouldBe List('b', 'd', 'e')
  }

  test("collect internal") {
    val ls = t.collectInternal
    pprint.log(ls)
    ls shouldBe List('a', 'c')
  }

}
