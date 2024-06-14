package problems

import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p64]]
  *
  * [[https://aperiodic.net/pip/scala/s-99/p64.svg]]
  */
object P64 {
  import P55._

  sealed trait PTree[+A]
  case class PNode[+A](value: A, l: PTree[A], r: PTree[A], x: Int, y: Int) extends PTree[A]
  case object PEnd extends PTree[Nothing]
  object PNode {
    def apply[A](value: A, x: Int, y: Int): PNode[A] = PNode(value, PEnd, PEnd, x, y)
  }

  def layout[A](t: Tree[A]): PTree[A] = {

    def go(t: Tree[A], x: Int, y: Int): (PTree[A], Int) = t match {
      case Node(value, l, r) =>
        val (lt, x1) = go(l, x, y + 1)
        val (xn, x2) = (x1, x1 + 1)
        val (rt, x3) = go(r, x2, y + 1)
        PNode(value, lt, rt, xn, y) -> x3
      case End               => PEnd -> x
    }

    go(t, x = 1, y = 1)._1
  }

}

class P64 extends Sandbox {
  import P57._
  import P64._

  val sample = bstFromList(List('n', 'k', 'm', 'c', 'a', 'h', 'g', 'e', 'u', 'p', 's', 'q'))

  test("layout") {
    val l = layout(sample)
    pprint.log(l)
    l shouldBe PNode(
      'n',
      l = PNode(
        'k',
        l = PNode(
          'c',
          l = PNode('a', l = PEnd, r = PEnd, x = 1, y = 4),
          r = PNode(
            'h',
            l = PNode(
              'g',
              l = PNode('e', l = PEnd, r = PEnd, x = 3, y = 6),
              r = PEnd,
              x = 4,
              y = 5
            ),
            r = PEnd,
            x = 5,
            y = 4
          ),
          x = 2,
          y = 3
        ),
        r = PNode('m', l = PEnd, r = PEnd, x = 7, y = 3),
        x = 6,
        y = 2
      ),
      r = PNode(
        'u',
        l = PNode(
          'p',
          l = PEnd,
          r = PNode(
            's',
            l = PNode('q', l = PEnd, r = PEnd, x = 10, y = 5),
            r = PEnd,
            x = 11,
            y = 4
          ),
          x = 9,
          y = 3
        ),
        r = PEnd,
        x = 12,
        y = 2
      ),
      x = 8,
      y = 1
    )
  }

}
