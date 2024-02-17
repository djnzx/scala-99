package problems

import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p39]] */
object P39 {
  import P31.primes

  def primesBetween(mn: Int, mx: Int): List[Int] =
    primes.dropWhile(_ < mn).takeWhile(_ <= mx).toList

}

class P39 extends Sandbox {
  import P39._

  test("1") {
    val data = Table(
      inOutHeader,
      7 -> 31 -> Seq(7, 11, 13, 17, 19, 23, 29, 31)
    )

    forAll(data) { case ((min, max), expected) =>
      primesBetween(min, max) shouldBe expected
    }
  }

}
