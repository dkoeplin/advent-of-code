import common.immutable.{Box, Pos}
import org.scalatest.flatspec._
import org.scalatest.matchers._

class TestBox extends AnyFlatSpec with should.Matchers {
  "Box" should "diff" in {
    def test(v: Box[Int], x: Pos[Int]): Unit = {
      val d = v diff Box.unit(x)
      val pts = d.flatMap(_.iterator.toSet)
      pts.size should be (26)
      d.foreach{m => assert(!m.contains(x), s"$v diff $x resulted in $m")  }
      pts.contains(x) should be (false)
    }
    val v = Box(Pos(0,0,0), Pos(2,2,2))
    v.iterator.foreach{i => test(v, i) }
  }

  "Box" should "have equal thickness borders" in {
    val v = Box(Pos(0,0), Pos(2,2))
    v.borders().foreach{_.thickness should be (1) }
  }

  "Box" should "iterate" in {
    val v = Box(Pos(0,0,0), Pos(2,2,2))
    val pts = (0 to 2).flatMap{i =>(0 to 2).flatMap{j => (0 to 2).map{k =>Pos(i, j, k) }}}
    v.iterator.size should be (27)
    v.iterator.toSeq should be (pts)
  }

  "Box" should "iterateBy" in {
    Box(Pos(0,0,0), Pos(2,2,2)).posIterator(Pos(2,2,2)).toList should be {
      List(Pos(0,0,0), Pos(0, 0, 2), Pos(0, 2, 0), Pos(0, 2, 2), Pos(2, 0, 0), Pos(2, 0, 2), Pos(2, 2, 0), Pos(2, 2, 2))
    }

    Box(Pos(0,-1024), Pos(2047, 1023)).posIterator(Pos(1024,1024)).toList should be {
      List(Pos(0,-1024), Pos(0,0), Pos(1024, -1024), Pos(1024, 0))
    }
  }

  "Box" should "alter" in {
    val v = Box(Pos(0,0), Pos(2,2))
    val rowIter = v.alter(0, 1, 1)
    rowIter should be (Box(Pos(1,0),Pos(1,2)))
    rowIter.size should be (3)
    rowIter.iterator.size should be (3)
    rowIter.iterator.toSeq should be (Seq(Pos(1,0),Pos(1,1),Pos(1,2)))
  }

  "Box" should "be unit" in {
    val u = Box.unit(Pos(0, 0))
    u.size should be (1)
    u.iterator.size should be (1)
    u.iterator.toSeq should be (Seq(Pos(0,0)))
  }
}
