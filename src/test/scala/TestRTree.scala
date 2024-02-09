import common.immutable.{Box, Pos}
import common.implicits.TupleOps._
import common.mutable.RTree
import common.traits.HasBox
import org.scalatest.flatspec._
import org.scalatest.matchers._

class TestRTree extends AnyFlatSpec with should.Matchers {
  case class LabeledBox(cube: Box[Int], id: Int) {
    override def toString: String = s"$id"
  }
  implicit class BoxOps(x: Box[Int]) {
    def named(id: Int): LabeledBox = LabeledBox(x, id)
  }
  implicit val LabeledCubeHasVolume: HasBox[Int,LabeledBox] = _.cube

  "RTree" should "floor" in {
    RTree.clamp((0, 0) to (511, 511), Pos(1024, 1024)) should be ((0,0) to (1023,1023))
    RTree.clamp((0, 0) to (1023, 1023), Pos(1024, 1024)) should be ((0,0) to (1023, 1023))
    RTree.clamp((0, 0) to (1024, 1024), Pos(1024, 1024)) should be ((0,0) to (2047, 2047))
    RTree.clamp((512, 512) to (1023, 1023), Pos(1024, 1024)) should be ((0,0) to (1023, 1023))
  }

  "RTree" should "add" in {
    val grid = RTree.empty[Int,LabeledBox](rank=2, maxEntries=1)
    grid += (10, 10) to (20, 40) named 1
    grid += (0,0) to (5,5) named 0
    grid.dump()
  }

  "RTree" should "subdivide" in {
    val grid = RTree.empty[Int,LabeledBox](rank=2, maxEntries=2)
    grid += (0, 0) to (511, 511) named 0
    grid += (512, 512) to (1023, 1023) named 1
    grid += (0, 512) to (511, 1023) named 2
    grid.size should be (3)
    var set = Set.empty[(Box[Int],Set[Int])]
    grid.iterate{(_,box,vs) => set = set + (box -> vs.map(_.id)) }
    set should be (Set(
      ((0, 0) to (511, 511)) -> Set(0),
      ((512,512) to (1023,1023)) -> Set(1),
      ((0, 512) to (511, 1023))  -> Set(2)
    ))
  }

  "RTree" should "bucket" in {
    val grid = RTree.empty[Int,LabeledBox](rank=2, maxEntries=10)
    grid += (0, 0) to (1512, 982) named 1
    grid += (0, 263) to (812, 881) named 2
    grid += (0, 223) to (824, 693) named 3
    grid += (0, 254) to (750, 613) named 4
    grid += (0, 175) to (801, 530) named 5
    grid += (0, 130) to (736, 356) named 6
    grid += (0, 107) to (702, 278) named 7
    grid += (0, 72) to (753, 202) named 8
    grid += (0, 373) to (433, 881) named 9
    grid += (0, 218) to (483, 811) named 10
    grid += (0, 284) to (1364, 881) named 11
    grid += (0, 203) to (1347, 698) named 12
    grid += (0, 45) to (1346, 539) named 13

    val spot = (98, 526) to (99, 527)
    grid.dump()
    grid.iterator.foreach{v => if (v.cube.overlaps(spot)) println(s"${v.id}: ${v.cube} => ${v.cube.intersect(spot).get}") }

  }

}
