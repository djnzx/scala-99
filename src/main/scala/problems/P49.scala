package problems

import org.scalatest.prop.TableFor1
import scala.annotation.tailrec
import tools.Sandbox

object P49 {
  val variants = List(0, 1)

  val base = List(Nil)
  def next(xs: List[List[Int]]): List[List[Int]] = variants.flatMap(d => xs.map(d :: _))

  val baseS = List("")
  def nextS(xs: List[String]): List[String] = variants.flatMap(d => xs.map(d + _))

  // classic recursive implementation
  def gray1(n: Int): List[List[Int]] = n match {
    case 0 => base
    case n =>
      val prev = gray1(n - 1)
      next(prev)
  }

  // classic recursive implementation, the same, different order
  def gray2(n: Int): List[List[Int]] = n match {
    case 0 => List(Nil)
    case n => gray2(n - 1).flatMap(l => variants.map(l :+ _))
  }

  // classic tail-recursive implementation
  def gray3(num: Int): List[List[Int]] = {

    @tailrec
    def go(n: Int, outcome: List[List[Int]]): List[List[Int]] = n match {
      case `num` => outcome
      case n     => go(n + 1, variants.flatMap(d => outcome.map(d :: _)))
    }

    go(0, List(Nil))
  }

  // recursive string based V1.1
  def gray4(n: Int): List[String] = n match {
    case 0 => baseS
    case n => nextS(gray4(n - 1))
  }

  // recursive string based V1.2
  def gray5(n: Int): List[String] = n match {
    case 0 => List("")
    case n => gray5(n - 1).flatMap { d => variants.map(d + _) }
  }

  // tail recursive, string
  def gray6(num: Int): List[String] = {

    @tailrec
    def go(n: Int, list: List[String]): List[String] = n match {
      case `num` => list
      case n     => go(n + 1, nextS(list))
    }

    go(0, List(""))
  }

  // iterative with var
  def gray7(num: Int): List[String] = {
    var n = 0
    var list = List("")
    while (n < num) {
      list = nextS(list)
      n += 1
    }
    list
  }

  // iterative without flatMap
  def gray8(num: Int) = {
    var n = 0
    var a = List("")
    while (n < num) {
      a = a.map(s => "0" + s).concat(a.map(s => "1" + s))
      n += 1
    }
    a
  }

  private val cache = scala.collection.mutable.Map(0 -> List(""))

  def gray9(n: Int): List[String] = {
    if (!cache.contains(n)) {
      val prev = gray9(n - 1)
      val curr = nextS(prev)
      cache.addOne(n -> curr)
    }

    cache(n)
  }

  // foldLeft
  def gray10(n: Int): List[String] = (1 to n).foldLeft(baseS)((a, _) => nextS(a))
}

class P49 extends Sandbox {
  import P49._

  test("next") {
    val data = Table(
      inOutHeader,
      List("")                     -> List("0", "1"),
      List("0", "1")               -> List("00", "01", "10", "11"),
      List("00", "01", "10", "11") -> List("000", "001", "010", "011", "100", "101", "110", "111")
    )

    forAll(data) { case (in, out) =>
      nextS(in) shouldBe out
    }
  }

  test("implementations") {
    val data = Table(
      inOutHeader,
      0 -> List(""),
      1 -> List("0", "1"),
      2 -> List("00", "01", "10", "11"),
      3 -> List("000", "001", "010", "011", "100", "101", "110", "111")
    )

    val impls: TableFor1[Int => List[String]] = Table(
      "impl",
      (gray1 _).andThen(_.map(_.mkString)),
      (gray2 _).andThen(_.map(_.mkString)),
      (gray3 _).andThen(_.map(_.mkString)),
      gray4,
      gray5,
      gray6,
      gray7,
      gray8,
      gray9,
      gray10
    )

    forAll(data) { case (in, out) =>
      forAll(impls) { impl =>
        impl(in) shouldBe out
      }
    }
  }
}
