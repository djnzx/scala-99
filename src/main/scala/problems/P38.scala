package problems

import tools.Sandbox
import tools.Timed.timed

/** [[https://aperiodic.net/phil/scala/s-99/#p38]] */
class P38 extends Sandbox {
  import P34._
  import P37._

  test("time") {
    val N = 10090000
    Seq(
      "Brute" -> timed(totient(N)),
      "Euler" -> timed(phi(N))
    ).foreach { case (m, (_, t)) =>
      val ms = s"${t}ms"
      pprint.pprintln(m -> ms)
    }
  }

}
