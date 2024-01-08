import common.implicits.IteratorOps._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class TestIteratorOps extends AnyFlatSpec with should.Matchers {
  "Iterator" should "split" in {
    val x = "AABAAABAA"
    Iterator.empty.mkString should be ("")
    x.iterator.split('B').map(_.mkString).toArray should be (Array("AA", "", "AAA", "", "AA"))

    val y = "ABBBBAABB"
    y.iterator.split('B').map(_.mkString).toArray should be (Array("A", "", "", "", "", "AA", "", ""))

    "BAB".iterator.split('B').map(_.mkString).toArray should be (Array("", "A", ""))
  }

  "Iterator" should "zipped" in {
    val a = 0 until 3
    val b = 2 until 5
    val c = 5 until 8
    val z = zipped(a.iterator, b.iterator, c.iterator)
    z.toSeq should be (Seq(Seq(0,2,5),Seq(1,3,6), Seq(2,4,7)))
  }
}
