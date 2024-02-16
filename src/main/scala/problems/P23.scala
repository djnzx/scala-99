package problems

import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p23]] */
object P23 {
  import P20.extractAt
  import P22.unfold
  import scala.util.Random

  def randZeroTo(n: Int): Int = Random.nextInt(n)

  def extractNrandom[A](n: Int, xs: List[A]): List[A] =
    unfold((n, xs.length, xs)) { case (n, len, xs) =>
      (n, len) match {
        case (0, _) => None
        case (_, 0) => throw new NoSuchElementException
        case (_, _) =>
          extractAt(randZeroTo(len), xs) match {
            case (xs, item) => Some((n - 1, len - 1, xs) -> item)
          }

      }
    }

}

class P23 extends Sandbox {
  import P23._

  test("normal") {
    val data: List[Char] = "QWERTYUIASDFGHJK".toList
    val items5: List[Char] = extractNrandom(5, data)
    items5.length shouldEqual 5
    data should contain allElementsOf items5
  }

  test("corner") {
    val data = "ABC".toList // len 3
    val num = 4
    a[NoSuchElementException] should be thrownBy extractNrandom(num, data)
  }
}
