package problems

import tools.Ex.neverByDesign

import scala.collection.immutable.LazyList.#::
import tools.Sandbox

/** Determine the prime factors of a given positive integer
  * it tries to divide by all prime numbers
  *
  * [[https://aperiodic.net/phil/scala/s-99/#p35]]
  */
object P35 {

  import P31.primes

  private def primeFactors(n: Int, primes: LazyList[Int], factors: List[Int]): List[Int] = (n, primes) match {
    case (1, _) => factors.sorted
    case (_, p #:: _) if n % p == 0 => primeFactors(n / p, primes, p :: factors)
    case (_, _ #:: ps) => primeFactors(n, ps, factors)
    case _             => neverByDesign // primes can't be empty
  }

  def primeFactors(n: Int): List[Int] = primeFactors(n, primes, Nil)

  def primeFactorsDistinct(n: Int): Set[Int] = primeFactors(n).toSet

}

class P35 extends Sandbox {
  import P35._

  test("non-distinct") {

    val data = Table(
      "in" -> "expected",
      315  -> List(3, 3, 5, 7)
    )

    forAll(data) { case (in, expected) =>
      primeFactors(in) shouldBe expected
    }

  }

  test("distinct") {

    val data = Table(
      "in" -> "expected",
      315  -> List(3, 5, 7)
    )

    forAll(data) { case (in, expected) =>
      primeFactorsDistinct(in) shouldBe expected.toSet
    }

  }
}
