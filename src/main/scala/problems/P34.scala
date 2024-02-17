package problems

import tools.PrettyOutput
import tools.Sandbox

/** Calculate Euler's totient function phi(m).
  * Brute Force implementation
  * for smart one -> [[P37]]
  *
  * [[https://aperiodic.net/phil/scala/s-99/#p34]]
  */
object P34 extends PrettyOutput {

  import P33.areCoPrime

  def totient(m: Int): Int =
    (1 to m)
      .count(areCoPrime(_, m))
//      .filter(areCoPrime(_, m))
//      .map(mprintln)
//      .size

}

class P34 extends Sandbox {
  import P34._

  test("1") {
    // 1,3,7,9
    totient(10) shouldEqual 4
  }

}
