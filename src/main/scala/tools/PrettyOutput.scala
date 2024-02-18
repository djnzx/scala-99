package tools

trait PrettyOutput {

  def line: Unit = println("-" * 50)
  def nl: Unit = println()

  def mprintln[A](a: A): A = {
    pprint.pprintln(a)
    a
  }

}
