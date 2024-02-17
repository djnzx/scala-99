package problems

import tools.Sandbox

/** A list of Goldbach compositions
  *
  * [[https://aperiodic.net/phil/scala/s-99/#p41]]
  */
object P41 {
  import P40._

  def goldbachList(from: Int, to: Int, min: Int = 1): Seq[(Int, (Int, Int))] =
    LazyList
      .from(from)
      .dropWhile(_ < 2)
      .filter(_ % 2 == 0)
      .takeWhile(_ <= to)
      .flatMap(n => goldbach(n).map(n -> _))
      .filter { case (_, (a, _)) => a >= min }

  implicit class PrettySyntax(private val seq: Seq[(Int, (Int, Int))]) extends AnyVal {
    def pretty = seq map { case (n, (a, b)) => s"$n = $a + $b" }
  }

}

class P41 extends Sandbox {
  import P41._

  test("min = 1") {
    goldbachList(9, 20).pretty
      .foreach(println)
  }

  test("min = 50") {
    goldbachList(1, 2000, 50).pretty
      .foreach(println)
  }
}
