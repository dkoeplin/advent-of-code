import common.immutable.Box
import common.implicits.TupleOps._
import common.mutable.RTree
import common.traits.HasBox
import org.scalatest.flatspec._
import org.scalatest.matchers._

class TestRTree extends AnyFlatSpec with should.Matchers {
  case class LabeledBox(cube: Box[Int], id: Int) {
    override def toString: String = s"$id: $cube"
  }
  implicit class CubeOps(x: Box[Int]) {
    def named(id: Int): LabeledBox = LabeledBox(x, id)
  }
  implicit val LabeledCubeHasVolume: HasBox[Int,LabeledBox] = _.cube

  "SparseGrid" should "add" in {
    val grid = RTree.empty[Int,LabeledBox](rank=2, maxEntries=1)
    grid += (10, 10) to (20, 40) named 1
    grid += (0,0) to (5,5) named 0
    grid.dump()
  }
}
