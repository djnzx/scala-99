package problems

import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p32]] */
object P32 {

  def gcd(a: Int, b: Int): Int = a % b match {
    case 0 => b
    case r => gcd(b, r)
  }

}

class P32 extends Sandbox {
  import P32._

  test("gcd") {
    val table = Table(
      "a, b"   -> "gcd",
      (1, 2)   -> 1,
      (10, 15) -> 5,
      (15, 5)  -> 5,
      (36, 24) -> 12,
      (63, 36) -> 9,
      (36, 63) -> 9
    )

    forAll(table) { case ((a, b), expected) =>
      gcd(a, b) shouldBe expected
    }
  }
}
