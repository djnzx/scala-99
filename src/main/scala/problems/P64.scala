package problems

import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p64]]
  *
  * [[https://aperiodic.net/pip/scala/s-99/p64.svg]]
  */
object P64 {
  import P55._

  case class At[A](value: A, x: Int, y: Int)

  def layout[A](t: Tree[A]): Tree[At[A]] = {

    def go(t: Tree[A], x: Int, level: Int): (Tree[At[A]], Int) = t match {
      case Node(value, l, r) =>
        val (lt, x1) = go(l, x, level + 1)
        val (xn, x2) = (x1, x1 + 1)
        val (rt, x3) = go(r, x2, level + 1)
        val at = At(value, xn, level)
        Node(at, lt, rt) -> x3
      case End               => End -> x
    }

    go(t, x = 1, level = 1)._1
  }

}

class P64 extends Sandbox {
  import P55._
  import P57._
  import P64._

  val sample = bstFromList(List('n', 'k', 'm', 'c', 'a', 'h', 'g', 'e', 'u', 'p', 's', 'q'))

  test("layout") {
    val l = layout(sample)
    pprint.log(l)
    l shouldBe Node(
      At('n', x = 8, y = 1),
      Node(
        At('k', x = 6, y = 2),
        Node(
          At('c', x = 2, y = 3),
          Node(At('a', x = 1, y = 4), End, End),
          Node(
            At('h', x = 5, y = 4),
            Node(
              At('g', x = 4, y = 5),
              Node(At('e', x = 3, y = 6), End, End),
              End
            ),
            End
          )
        ),
        Node(At('m', x = 7, y = 3), End, End)
      ),
      Node(
        At('u', x = 12, y = 2),
        Node(
          At('p', x = 9, y = 3),
          End,
          Node(
            At('s', x = 11, y = 4),
            Node(At('q', x = 10, y = 5), End, End),
            End
          )
        ),
        End
      )
    )
  }

}
