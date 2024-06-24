package problems

import tools.Sandbox

/** - tree height
  * - tree right rotation
  * - tree left rotation
  * - tree is balanced
  * - tree balance
  */
object P56A {
  import P55._

  def height(t: Tree[_]): Int = t match {
    case End           => 0
    case Node(_, l, r) => 1 + (height(l) max height(r))
  }

  final class WrongTreeStructure extends RuntimeException

  @throws[WrongTreeStructure]
  def rotateRight[A](t: Tree[A]): Tree[A] = t match {
    case Node(d, Node(b, a, c), e) => Node(b, a, Node(d, c, e))
    case _                         => throw new WrongTreeStructure
  }

  @throws[WrongTreeStructure]
  def rotateLeft[A](t: Tree[A]): Tree[A] = t match {
    case Node(b, a, Node(d, c, e)) => Node(d, Node(b, a, c), e)
    case _                         => throw new WrongTreeStructure
  }

  def heights(t: Tree[_]): (Int, Int) = t match {
    case End           => (0, 0)
    case Node(_, l, r) => height(l) -> height(r)
  }

  def isBalanced(t: Tree[_]): Boolean = heights(t) match {
    case (l, r) => math.abs(l - r) <= 1
  }

  def balance[A](t: Tree[A]): Tree[A] = heights(t) match {
    case (lh, rh) if lh - rh > 1 => rotateRight(t)
    case (lh, rh) if rh - lh > 1 => rotateLeft(t)
    case _                       => t
  }

}

class P56A extends Sandbox {
  import P55._
  import P56A._

  test("height - 0") {
    val t0 = End
    height(t0) shouldBe 0
    heights(t0) shouldBe (0, 0)
  }

  test("height - 1") {
    val t0 = Node('a')
    height(t0) shouldBe 1
    heights(t0) shouldBe (0, 0)
  }

  test("height - 2") {
    val t0 = Node('a', Node('b'), Node('c'))
    height(t0) shouldBe 2
    heights(t0) shouldBe (1, 1)
  }

  test("height - 2L") {
    val t0 = Node('a', Node('b'), End)
    height(t0) shouldBe 2
    heights(t0) shouldBe (1, 0)
  }

  test("height - 2R") {
    val t0 = Node('a', End, Node('c'))
    height(t0) shouldBe 2
    heights(t0) shouldBe (0, 1)
  }

  test("rotate right - success") {
    val t = Node('d', Node('b', 'a', 'c'), Node('e'))
    val r = Node('b', Node('a'), Node('d', 'c', 'e'))

    rotateRight(t) shouldBe r
  }

  test("rotate right - wrong structure") {
    val t = Node('a', End, Node('c'))
    a[WrongTreeStructure] shouldBe thrownBy(rotateRight(t))
  }

  test("rotate left - success") {
    val t = Node('b', Node('a'), Node('d', 'c', 'e'))
    val l = Node('d', Node('b', 'a', 'c'), Node('e'))
    rotateLeft(t) shouldBe l
  }

  test("rotate left - wrong structure") {
    val t = Node('a', Node('b'), End)
    a[WrongTreeStructure] shouldBe thrownBy(rotateLeft(t))
  }

  test("balance") {
    val t0 = Node(
      'a',
      Node('b', Node('c'), End),
      End
    )
    val exp = Node('b', 'c', 'a')

    val t2 = balance(t0)
    pprint.log(t0)
    pprint.log(t2)

    t2 shouldBe exp
  }

}
