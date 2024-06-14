package problems

import tools.Sandbox

object P64A {
  import P55._

  /** in-order traversal: L,N,R */
  def tInOrder[A](t: Tree[A]): List[A] = t match {
    case Node(value, l, r) => tInOrder(l) ::: value :: tInOrder(r)
    case End               => Nil
  }

  /** pre-order traversal: N,L,R */
  def tPreOrder[A](t: Tree[A]): List[A] = t match {
    case Node(value, l, r) => value :: tPreOrder(l) ::: tPreOrder(r)
    case End               => Nil
  }

  /** post-order traversal L,R,N */
  def tPostOrder[A](t: Tree[A]): List[A] = t match {
    case Node(value, l, r) => tPostOrder(l) ::: tPostOrder(r) ::: List(value)
    case End               => Nil
  }

}

class P64A extends Sandbox {
  import P57._
  import P64A._

  val bst = bstFromList(List('n', 'k', 'm', 'c', 'a', 'h', 'g', 'e', 'u', 'p', 's', 'q'))

  test("in-order traversal") {
    val xs = tInOrder(bst)
    pprint.log(xs)
    xs shouldBe "aceghkmnpqsu".toList
  }

  test("pre-order traversal") {
    val xs = tPreOrder(bst)
    pprint.log(xs)
    xs shouldBe "nkcahgemupsq".toList
  }

  test("post-order traversal") {
    val xs = tPostOrder(bst)
    pprint.log(xs)
    xs shouldBe "aeghcmkqspun".toList
  }

}
