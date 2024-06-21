package tools

trait Timed {

  def now: Long = System.currentTimeMillis

  def timed[A](body: => A): (A, Long) = {
    val started = now
    val a: A = body
    val spent = now - started
    (a, spent)
  }

}

object Timed extends Timed
