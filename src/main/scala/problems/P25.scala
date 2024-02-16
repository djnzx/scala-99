package problems

import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p25]] */
object P25 {
  import P23.extractNrandom

  def permutate[A](xs: List[A]) =
    extractNrandom(xs.length, xs)

}

class P25 extends Sandbox {
  import P25._

  test("1") {
    val MAX = 20
    val original = (1 to MAX).toList
    val permutated = permutate(original)

    permutated.length shouldBe original.length
    original should contain allElementsOf permutated

    println(original)
    println(permutated)
  }
}
