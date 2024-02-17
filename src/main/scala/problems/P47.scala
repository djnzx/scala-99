package problems

import tools.Sandbox

/** attach syntax
  * [[https://aperiodic.net/phil/scala/s-99/#p47]]
  */
object P47 {

  import P46._

  def not(b: Boolean): Boolean = !b

  implicit class BoolOps(val a: Boolean) extends AnyVal {
    def and(b: Boolean): Boolean = andF(a, b)
    def nand(b: Boolean): Boolean = nandF(a, b)
    def or(b: Boolean): Boolean = orF(a, b)
    def nor(b: Boolean): Boolean = norF(a, b)
    def xor(b: Boolean): Boolean = xorF(a, b)
    def impl(b: Boolean): Boolean = implF(a, b)
    def equ(b: Boolean): Boolean = equF(a, b)
  }

}

class P47 extends Sandbox {
  import P47._

  test("1") {
    true and true shouldBe true
    true and (true or false) shouldBe true
  }

}
