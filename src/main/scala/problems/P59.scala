package problems

import tools.Sandbox

/** [[https://aperiodic.net/phil/scala/s-99/#p59]] */
object P59 {
  import P55._

  def mkHeightBalanced[A](value: A, h: Int): LazyList[Tree[A]] = h match {
    case 0 => LazyList(End)
    case 1 => LazyList(Node(value))
    case h =>
      val full = mkHeightBalanced(value, h - 1)
      val short = mkHeightBalanced(value, h - 2)
      val tt1 = full.flatMap(lt => full.map(rt => Node(value, lt, rt)))
      val tt2 = full.flatMap { ft =>
        short.flatMap { st =>
          LazyList(Node(value, ft, st), Node(value, st, ft))
        }
      }
      tt1 ++ tt2
  }

}

class P59 extends Sandbox {
  import P59._

  test("all height balanced trees of height") {
    val data = Table(
      inHeader,
//      0, // 1
//      1, // 1
//      2, // N(h-1)×N(h-2) + N(h-2)×N(h-1) + N(h-1)×N(h-1) = 1 + 1 + 1 = 3
      3 // 3×3 + 3×1 + 3×1 = 15
//      4, // 15*15 + 15*3+15*3 = 315
//      5, // 315*315 + 315*15*2 = 108675
//      6, // 108675*108675 + 108675*315*2 = 11'878'720'875
    )

    forAll(data) { h =>
      val ts = mkHeightBalanced("a", h)
      pprint.log(h -> ts, showFieldNames = false)
      val sizes = ts.map(_.size).distinct.sorted
      pprint.log((h, ts.size, s"${sizes.head}...${sizes.last}"))

    }
  }

}
