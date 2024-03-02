import common.immutable.Pos
import common.implicits.TupleOps._
import common.mutable.RTree
import org.scalatest.flatspec._
import org.scalatest.matchers._

class TestRTree extends AnyFlatSpec with should.Matchers with LabeledBoxes {
  "RTree" should "clampDown" in {
    import RTree.clampDown
    clampDown(Pos(0, -1024), Pos(1024, 1024)) should be (Pos(0, -1024))
  }

  "RTree" should "clamp" in {
    import RTree.clamp
    clamp((0, 0) to (511, 511), Pos(1024, 1024)) should be ((0,0) to (1023,1023))
    clamp((0, 0) to (1023, 1023), Pos(1024, 1024)) should be ((0,0) to (1023, 1023))
    clamp((0, 0) to (1024, 1024), Pos(1024, 1024)) should be ((0,0) to (2047, 2047))
    clamp((512, 512) to (1023, 1023), Pos(1024, 1024)) should be ((0,0) to (1023, 1023))
    clamp((346, -398) to (666, -202), Pos(1024, 1024)) should be ((0,-1024) to (1023,-1))
    clamp((-100, 100) to (100, 300), Pos(1024, 1024)) should be ((-1024, 0) to (1023, 1023))
  }

  "RTree" should "add" in {
    val tree = RTree.empty[Int,LabeledBox](rank=2, maxEntries=1)
    tree += (10, 10) to (20, 40) named 1
    tree += (0,0) to (5,5) named 0
    tree.dump()
  }

  "RTree" should "subdivide" in {
    val tree = RTree.empty[Int,LabeledBox](rank=2, maxEntries=2)
    tree += (0, 0) to (511, 511) named 0
    tree += (512, 512) to (1023, 1023) named 1
    tree += (0, 512) to (511, 1023) named 2
    tree.size should be (3)
    collectIDs(tree) should be (Map(
      ((0, 0) to(511, 511)) -> Set(0),
      ((512, 512) to(1023, 1023)) -> Set(1),
      ((0, 512) to(511, 1023)) -> Set(2)
    ))
  }

  "RTree" should "create new bucket" in {
    val tree = RTree.empty[Int,LabeledBox](rank=2, maxEntries=10)
    tree += (((0, 882) to (1512, 982)) + (0, 0)) named 1
    tree += (((485, 211) to (762, 881)) + (0, 78)) named 2
    tree += (((527, -173) to (748, 210)) + (0, 125)) named 3
    tree.dump()
  }

  "RTree" should "bucket" in {
    val tree = RTree.empty[Int,LabeledBox](rank=2, maxEntries=10)
    tree += (0, 0) to (1512, 982) named 1
    tree += (0, 263) to (812, 881) named 2
    tree += (0, 223) to (824, 693) named 3
    tree += (0, 254) to (750, 613) named 4
    tree += (0, 175) to (801, 530) named 5
    tree += (0, 130) to (736, 356) named 6
    tree += (0, 107) to (702, 278) named 7
    tree += (0, 72) to (753, 202) named 8
    tree += (0, 373) to (433, 881) named 9
    tree += (0, 218) to (483, 811) named 10
    tree += (0, 284) to (1364, 881) named 11
    tree += (0, 203) to (1347, 698) named 12
    tree += (0, 45) to (1346, 539) named 13

    val spot = (98, 526) to (99, 527)
    tree.dump()
    tree.iterator.foreach{v => if (v.box.overlaps(spot)) println(s"${v.id}: ${v.box} => ${v.box.intersect(spot).get}") }
  }

  "RTree" should "maintain buckets after subdivide" in {
    val tree = RTree.empty[Int,LabeledBox](rank=2, maxEntries=10)
    tree += (((0, 882) to (1512, 982))  named 1) + Pos(0, 0)
    tree += (((614, 762) to (762, 881)) named 2) + Pos(0, 253)
    tree += (((594, 701) to (715, 761)) named 3) + Pos(0, 232)
    tree += (((620, 641) to (684, 700)) named 4) + Pos(0, 183)
    tree += (((616, 592) to (686, 640)) named 5) + Pos(0, 137)
    tree += (((603, 536) to (680, 591)) named 6) + Pos(0, 132)
    tree += (((582, 474) to (662, 535)) named 7) + Pos(0, 130)
    tree += (((615, 416) to (672, 473)) named 8) + Pos(0, 82)
    tree += (((599, 375) to (647, 415)) named 9) + Pos(0, 41)
    tree += (((512, 512) to (514, 514)) named 10)
    collectIDs(tree) should be (Map(
      ((1024,0) to (2047, 1023)) -> Set(1),
      ((0, 1024) to (1023, 2047)) -> Set(2),
      ((512, 512) to (1023, 1023)) -> Set(1, 2, 3, 4, 5, 6, 7, 8, 10),
      ((0, 512) to (511, 1023)) -> Set(1),
      ((512, 0) to (1023, 511)) -> Set(8, 9)
    ))
  }

  "RTree" should "create negative bucket" in {
    val tree = RTree.empty[Int,LabeledBox](rank=2, maxEntries=10)
    tree += (((0, 882) to (1512, 982)) + (0, 0)) named 1
    tree += (((346, -398) to (666, -202)) + (0, 0)) named 2
    collectIDs(tree) should be (Map(
      ((0, -1024) to (1023, -1)) -> Set(2),
      ((1024, 0) to (2047, 1023)) -> Set(1),
      ((0, 0) to (1023, 1023)) -> Set(1)
    ))
  }

  "RTree" should "fetch" in {
    val tree = RTree.empty[Int,LabeledBox](rank=2, maxEntries=10)
    val a = (((0, 882) to (1512, 982)) + (0, 0)) named 1
    val b = (((346, -398) to (666, -202)) + (0, 0)) named 2
    tree += a
    tree += b

    tree((0, -300) to (1024, 1000)) should be (Set(a, b))
    tree((0, 0) to (100, 100)) should be (Set.empty)
    tree((0, 885) to (100, 886)) should be (Set(a))
  }

  "RTree" should "get components" in {
    val tree = RTree.empty[Int, LabeledBox](rank = 2, maxEntries = 10)
    val a = ((0, 0) to(100, 100)) named 1
    val b = ((0, 101) to(100, 200)) named 2
    tree += a
    tree += b
    tree.components().toList should be(List(Set(a, b)))
  }
}
