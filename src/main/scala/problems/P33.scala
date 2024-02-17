package problems

import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p33]] */
object P33 {
  import P32._

  def areCoPrime(a: Int, b: Int) = gcd(a, b) == 1

}

class P33 extends Sandbox {
  import P33._

  test("1") {

    val f = Seq(
      (10, 15),
      (15, 5),
      (36, 24),
      (63, 36)
    ).map(_ -> false)

    val t = Seq(
      (1, 93),
      (17, 39),
      (35, 64),
      (101, 71)
    ).map(_ -> true)

    val data = Table(
      "a, b" -> "CoPrime",
      t ++ f: _*
    )

    forAll(data) { case ((a, b), expected) =>
      areCoPrime(a, b) shouldEqual expected
    }
  }
}
