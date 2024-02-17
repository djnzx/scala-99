package tools

trait PrettyOutput {

  def mprintln[A](a: A): A = {
    pprint.pprintln(a)
    a
  }

}
