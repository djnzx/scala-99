package problems

import tools.Sandbox

/** Goldbach's conjecture says that
  * every positive even number greater than 2 is the sum of two prime numbers.
  * E.g. 28 = 5 + 23.
  *
  * It is one of the most famous facts in number theory that
  * has not been proved to be correct in the general case.
  * It has been numerically confirmed up to very large numbers
  *
  * [[https://aperiodic.net/phil/scala/s-99/#p40]]
  */
object P40 {
  import P31.isPrime
  import P31.primes

  def goldbach(n: Int): Option[(Int, Int)] =
    Some(primes)
      .filterNot(_ => n <= 0)
      .filterNot(_ => n % 2 != 0)
      .flatMap(
        _.takeWhile(_ < n)
          .find(x => isPrime(n - x))
      )
      .map(x => (x, n - x))

}

class P40 extends Sandbox {
  import P40._

  test("1") {
    val data = Table(
      inOutHeader,
      28     -> (5, 23),
      786876 -> (17, 786859)
    )

    forAll(data) { case (n, expected) =>
      goldbach(n) shouldBe Some(expected)
    }
  }
}
