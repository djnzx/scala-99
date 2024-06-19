package problems

import cats.implicits.catsSyntaxEitherId
import cats.parse.Parser
import cats.parse.Parser0
import tools.Sandbox

/** [[https://aperiodic.net/pip/scala/s-99/#p67]]
  */
object P67 {
  import P55._

  def stringify[A](t: Tree[A]): String = t match {
    case End                   => ""
    case Node(value, End, End) => value.toString
    case Node(value, l, r)     => s"$value(${stringify(l)},${stringify(r)})"
  }

  object p {
    import cats.parse.Parser.char
    import cats.parse.Parser.charIn

    /** valueParser */
    val value = charIn('a' to 'z')
    val comma = char(',')

    /** whatever wrapped into parenthesis */
    def br[A](pa: Parser[A]) = pa.between(char('('), char(')'))

    /** a(b,c) - full node - Node(a, Node(b), Node(c)) */
    def fullNode = Parser.defer(value ~ br(node.with1 ~ (comma *> node)))
      .map { case (n, (l, r)) => Node(n, l, r) }

    /** a(b,) - partial left node - Node(a, Node(b), End) */
    def partLeftNode = Parser.defer(value ~ br(node.with1 <* comma))
      .map { case (n, l) => Node(n, l, End) }

    /** a(,c) - partial right node - Node(a, End, Node(c)) */
    def partRightNode = Parser.defer(value ~ br(comma *> node))
      .map { case (n, r) => Node(n, End, r) }

    /** a - terminal node (value lifted to the node) - Node(a, End,End) */
    def terminalNode = value
      .map(x => Node(x, End, End))

    val end = Parser.unit
      .as(End: Tree[Nothing])

    def node: Parser0[Tree[Char]] =
      fullNode.backtrack | partLeftNode.backtrack | partRightNode.backtrack | terminalNode.backtrack | end

    def tree = node
  }

  def parse(raw: String): Tree[Char] =
    p.tree
      .parseAll(raw)
      .fold(
        e => throw new RuntimeException(e.toString),
        identity
      )

}

class P67 extends Sandbox {
  import P55._
  import P67._

  test("1. node - empty") {
    val raw = ""
    val t = End

    p.end.parseAll(raw) shouldBe t.asRight
  }

  test("2. node - terminal") {
    val raw = "a"
    val t = Node('a', End, End)

    p.terminalNode.parseAll(raw) shouldBe t.asRight
    p.node.parseAll(raw) shouldBe t.asRight
  }

  test("3. node - partial left") {
    val raw = "a(b,)"
    val t = Node('a', Node('b'), End)

    p.partLeftNode.parseAll(raw) shouldBe t.asRight
    p.node.parseAll(raw) shouldBe t.asRight
  }

  test("4. node - partial left") {
    val raw = "a(b(c,),)"
    val t = Node('a', Node('b', Node('c'), End), End)

    p.partLeftNode.parseAll(raw) shouldBe t.asRight
    p.node.parseAll(raw) shouldBe t.asRight
  }

  test("5. node - partial right") {
    val raw = "a(,c)"
    val t = Node('a', End, Node('c'))

    p.partRightNode.parseAll(raw) shouldBe t.asRight
    p.node.parseAll(raw) shouldBe t.asRight
  }

  test("6. node - full") {
    val raw = "a(b,c)"
    val t = Node('a', Node('b'), Node('c'))

    p.fullNode.parseAll(raw) shouldBe t.asRight
    p.node.parseAll(raw) shouldBe t.asRight
  }

  val tree: Node[Char] =
    Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End)))

  val raw = "a(b(d,e),c(,f(g,)))"

  test("stringify") {
    stringify(tree) shouldBe raw
  }

  test("parse") {
    parse(raw) shouldBe tree
  }

}
