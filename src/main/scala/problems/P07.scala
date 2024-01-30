package problems

import scala.annotation.tailrec
import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p07]] */
object P07 {

  // one level, unsafe
  def flatten1(xs: List[Any]): List[Any] = {

    @tailrec
    def go(xs: List[Any], acc: List[Any]): List[Any] = xs match {
      case Nil    => acc
      case h :: t =>
        h match {
          case n: Int       => go(t, acc :+ n)
          case l: List[Any] => go(t, acc ++ l)
          case x            => go(t, acc :+ x)
        }
    }

    go(xs, Nil)
  }

  // any levels - unsafe
  def flattenN(xs: List[Any]): List[Any] = xs match {
    case Nil    => Nil
    case h :: t =>
      h match {
        case n: Int       => n :: flattenN(t)
        case l: List[Any] => flattenN(l) ++ flattenN(t)
        case x            => x :: flattenN(t)
      }
  }

  // to express it in a safe way we need the common type
  // what's interesting here - we can't express more than one level od nesting
  sealed trait LA[A]
  case class LAItem[A](a: A) extends LA[A]
  case class LAList[A](as: List[A]) extends LA[A]

  def flatten1s[A](xs: List[LA[A]]): List[A] = {

    @tailrec
    def go(xs: List[LA[A]], acc: List[A]): List[A] = xs match {
      case Nil             => acc
      case LAItem(a) :: t  => go(t, acc :+ a)
      case LAList(as) :: t => go(t, acc ++ as)
    }

    go(xs, Nil)
  }

  def flatten1sr[A](xs: List[LA[A]]): List[A] = xs match {
    case Nil             => Nil
    case LAItem(a) :: t  => a :: flatten1sr(t)
    case LAList(as) :: t => as ++ flatten1sr(t)
  }

  // to express it in a safe way for a unlimited level of nesting
  // we need to use recursive type
  // what's interesting here - we can't express the wrong structure
  sealed trait LAR[A]
  case class LAItemR[A](a: A) extends LAR[A]
  case class LAListR[A](as: List[LAR[A]]) extends LAR[A]

  def flattenNs[A](xs: List[LAR[A]]): List[A] = xs match {
    case Nil              => Nil
    case LAItemR(a) :: t  => a :: flattenNs(t)
    case LAListR(as) :: t => flattenNs(as) ++ flattenNs(t)
  }

}

class P07 extends Sandbox {
  import P07._

  test("one level, UNSAFE") {
    val xs = List(
      List(1, 1),
      2,
      List(3, List(5, 8))
    )
    flatten1(xs) shouldEqual List(1, 1, 2, 3, List(5, 8))
  }

  test("any levels - UNSAFE") {
    val xs = List(List(1, List(1)), 2, List(3, List(5, List(8))))
    flattenN(xs) shouldEqual List(1, 1, 2, 3, 5, 8)
  }

  test("one level - SAFE") {
    flatten1s(
      List(
        LAItem(1),
        LAList(List(2, 3))
      )
    ) shouldBe List(1, 2, 3)
  }

  test("any levels - SAFE") {
    flattenNs(
      List(
        LAItemR(1),
        LAListR(List(LAItemR(2), LAItemR(3))),
        LAListR(
          List(
            LAListR(List(LAItemR(4))),
            LAItemR(5)
          )
        )
      )
    ) shouldBe List(1, 2, 3, 4, 5)
  }

}
