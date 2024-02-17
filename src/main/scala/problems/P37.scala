package problems

import tools.Sandbox

/** Calculate Euler's totient function phi(m)
  * Smart implementation
  * for brute force one -> [[P34]]
  *
  * [[https://aperiodic.net/phil/scala/s-99/#p37]]
  */
object P37 {
  import P36._

  /** a^b^ */
  def ab(a: Int, b: Int): Long = (1 to b).foldLeft(1L) { (acc, _) => acc * a }

  /** (m-1)*m^(p-1)^ */
  def mp(m: Int, p: Int): Long = (m - 1).toLong * ab(m, p - 1)

  /** phi(m) = (p1-1)*p1^(m1-1)^^ * (p2-1)*p2^(m2-1)^^ * (p3-1)*p3^(m3-1)^ * ... */
  def phi(n: Int): Long =
    primeFactorMultiplicity(n).foldLeft(1L) { case (acc, (m, p)) => acc * mp(m, p) }

}

class P37 extends Sandbox {
  import P37._

  test("a^b") {
    val data = Table(
      inOutHeader,
      17  -> 1 -> 17,
      111 -> 0 -> 1,
      2   -> 3 -> 8,
      3   -> 3 -> 27
    )

    forAll(data) { case ((a, b), r) =>
      ab(a, b) shouldEqual r
    }
  }

  test("mp") {
    val data = Table(
      inOutHeader,
      2 -> 1 -> 1,
      5 -> 1 -> 4,
      7 -> 3 -> 294
    )

    forAll(data) { case ((a, b), r) =>
      mp(a, b) shouldEqual r
    }
  }

  test("phi") {
    phi(10) shouldEqual 4
  }

}
