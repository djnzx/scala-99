package problems

import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p24]] */
object P24 {

  import P22.rangeUnfold
  import P23.extractNrandom

  def lotto(cnt: Int, mx: Int) =
    extractNrandom(cnt, rangeUnfold(1, mx))
    
}

class P24 extends Sandbox {
  import P24._

  test("normal") {
    val all = (1 to 49).toList
    
    val actual = lotto(6, 49)

    actual.length shouldBe 6
    all should contain allElementsOf actual
  }

}