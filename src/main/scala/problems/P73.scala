package problems

import cats.parse.Parser
import cats.parse.Parser.char
import cats.parse.Rfc5234.wsp
import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p73]]
  *
  * [[https://aperiodic.net/pip/scala/s-99/p73.svg]]
  */
object P73 {

  import P70._

  def lispy[A](t: MTree[A]): String = t match {
    case MTree(x, Nil) => x.toString
    case MTree(x, ch)  => "(" concat (x.toString :: ch.map(lispy)).mkString(" ") concat ")"
  }

  class p[A](value: Parser[A]) {

    def br[B](pa: Parser[B]) = pa.between(char('('), char(')'))

    val emptyNode: Parser[MTree[A]] = value.map(x => MTree(x))

    val nonEmptyNode: Parser[MTree[A]] = Parser.defer(
      br(
        (value ~ (wsp *> node).rep).map {
          case (value, ch) => MTree(value, ch.toList)
        }
      )
    )

    val node: Parser[MTree[A]] = emptyNode.backtrack | nonEmptyNode

  }

  def parse0[A](valueParser: Parser[A])(raw: String): MTree[A] =
    new p(valueParser)
      .node
      .parseAll(raw)
      .fold(
        e => throw new RuntimeException(e.toString),
        identity
      )

  val charParser: Parser[Char] = Parser.charIn('a' to 'z')

  object p extends p(charParser)

  def parseLispy = parse0(charParser) _

}

class P73 extends Sandbox {

  import P70._
  import P73._

  test("1") {
    val t0 = MTree('a')
    val lsp = "a"
    lispy(t0) shouldBe lsp
    parseLispy(lsp) shouldBe t0
  }

  test("2") {
    val t0 = MTree('a', MTree('b'))
    val lsp = "(a b)"
    lispy(t0) shouldBe lsp
    parseLispy(lsp) shouldBe t0
  }

  test("3") {
    val t0 = MTree('a', MTree('b', MTree('c')))
    val lsp = "(a (b c))"
    lispy(t0) shouldBe lsp
    parseLispy(lsp) shouldBe t0
  }

  test("4") {
    val t0 = MTree('b', MTree('d'), MTree('e'))
    val lsp = "(b d e)"
    lispy(t0) shouldBe lsp
    parseLispy(lsp) shouldBe t0
  }

  test("5") {
    val t0 = MTree(
      'a',
      List(
        MTree('f', List(MTree('g'))),
        MTree('c'),
        MTree('b', List(MTree('d'), MTree('e')))
      )
    )
    val lsp = "(a (f g) c (b d e))"
    lispy(t0) shouldBe lsp
    parseLispy(lsp) shouldBe t0
  }

}
