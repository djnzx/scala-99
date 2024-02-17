package problems

import tools.Sandbox

/** Truth tables for logical expressions
  * `and`, `or`, `nand`, `nor`, `xor`, `impl`, and `equ`
  *
  * [[https://aperiodic.net/phil/scala/s-99/#p46]]
  */
object P46 {
  type BBB = (Boolean, Boolean) => Boolean

  trait BF2 extends BBB

  object BF2 {
    def apply(op0: BBB, show: String): BF2 = new BF2 {
      override def apply(v1: Boolean, v2: Boolean): Boolean = op0(v1, v2)
      override def toString: String = show
    }
  }

  val andF = BF2(
    _ & _,
    "AND"
  )
  val nandF = BF2(
    (x, y) => !(x & y),
    "NAND"
  )
  val orF = BF2(
    _ | _,
    "OR"
  )
  val norF = BF2(
    (x, y) => !(x | y),
    "NOR"
  )
  val xorF = BF2(
    _ ^ _,
    "XOR"
  )
  // https://calcworkshop.com/logic/logical-implication/
  val implF = BF2(
    {
      case (false, false) => true
      case (false, true)  => true
      case (true, true)   => true
      case (true, false)  => false
    },
    "IMPL"
  )
  // A == B
  // ! (A ^ B)
  // (A => B) & (B => A)
  val equF = BF2(
    (x, y) => !(x ^ y),
    "EQU"
  )
  val equ2 = BF2(
    (x, y) => implF.apply(x, y) & implF.apply(y, x),
    "EQU2"
  )

  def booleans = Seq(true, false)

  implicit class BoolFormatted(private val x: Boolean) extends AnyVal {
    def rp = "%-6s".format(x)
  }

  def header(f: BF2) = s"  A      B     ${f.toString}  "

  def table2wo(f: BF2) =
    for {
      a <- booleans
      b <- booleans
      r = f(a, b)
    } yield s"${a.rp} ${b.rp} ${r.rp}"

  def table2(f: BF2) = header(f) +: table2wo(f) :+ ""

}

class P46 extends Sandbox {
  import P46._

  test("1") {
    val x = Seq(orF, norF, andF, nandF, xorF, equF, equ2, implF)
      .map(f => table2(f).mkString("\n"))
      .mkString("\n")

    println(x)
  }
}
