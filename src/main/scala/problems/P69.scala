package problems

import cats.implicits.toComposeOps
import problems.P64A.tPreOrder
import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p69]]
  */
object P69 {
  import P55._
  import P67._

  /** preorder with dots instead of End */
  def treeToDots(t: Tree[Char]): String = t match {
    case End               => "."
    case Node(value, l, r) => value.toString + treeToDots(l) + treeToDots(r)
  }

  def parensToDots: String => String =
    P67.parse >>> treeToDots

  def dotsToTree(dots: String): Tree[Char] = {

    /** classic List decomposition */
    def reconstructList(dots: List[Char]): (Tree[Char], List[Char]) = dots match {
      case Nil       => End -> Nil
      case '.' :: xs => End -> xs
      case n :: t0   =>
        val (l, t1) = reconstructList(t0)
        val (r, t2) = reconstructList(t1)
        Node(n, l, r) -> t2
    }

    /** optimized index manipulation */
    def reconstruct(idx: Int): (Tree[Char], Int) = dots(idx) match {
      case '.' => End -> (idx + 1)
      case n   =>
        val (l, idx2) = reconstruct(idx + 1)
        val (r, idx3) = reconstruct(idx2)
        Node(n, l, r) -> idx3
    }

//    reconstructList(dots.toList)._1
    reconstruct(0)._1
  }

  def dotsToParens: String => String =
    dotsToTree _ >>> stringify

}

class P69 extends Sandbox {

  import P55._
  import P64A._
  import P69._

  val parens = "a(b(d,e),c(,f(g,)))"
  val dots = "abd..e..c.fg..."
  val t = P67.parse(parens)

  /*
    Node(
      'a',
      l = Node(
        'b',
        l = Node('d', l = End, r = End),
        r = Node('e', l = End, r = End)
      ),
      r = Node(
        'c',
        l = End,
        r = Node(
          'f',
          l = Node(value = 'g', l = End, r = End),
          r = End
        )
      )
    )

   */
  ignore("tree") {
    pprint.log(t)
  }

  test("() -> ...") {
    parensToDots(parens) shouldBe dots
  }

  test("... -> ()") {
    dotsToParens(dots) shouldBe parens
  }

}
