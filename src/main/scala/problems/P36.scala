package problems

import tools.Sandbox

/** Determine the prime factors of a given positive integer
  * [[https://aperiodic.net/phil/scala/s-99/#p36]]
  */
object P36 {

  import P10._
  import P35._

  def primeFactorMultiplicity1(a: Int): Map[Int, Int] =
    pack(primeFactors(a)).toMap

  def primeFactorMultiplicity2(n: Int): Map[Int, Int] =
    primeFactors(n)
      .groupMapReduce(identity)(_ => 1)(_ + _)

}

class P36 extends Sandbox {
  import P36._

  test("with count") {

    val data = Table(
      inOutHeader,
      8   -> Map(2 -> 3),
      24  -> Map(2 -> 3, 3 -> 1),
      315 -> Map(3 -> 2, 5 -> 1, 7 -> 1)
    )

    forAll(data) { case (in, expected) =>
      primeFactorMultiplicity1(in) shouldBe expected
      primeFactorMultiplicity2(in) shouldBe expected
    }
  }

}
