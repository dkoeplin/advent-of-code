import common.immutable.{Box, Pos}
import common.mutable.RTree
import common.traits.RTreeHash

trait LabeledBoxes {
  case class LabeledBox(box: Box[Int], id: Int) {
    def +(rhs: Pos[Int]): LabeledBox = LabeledBox(box + rhs, id)
    override def toString: String = s"$id"
  }
  object LabeledBox {
    implicit val kLabeledBoxHasBox: RTreeHash[Int,LabeledBox] = new RTreeHash[Int,LabeledBox]{
      override def hash(value: LabeledBox): Int = value.id
      override def box(value: LabeledBox): Box[Int] = value.box
      override def move(value: LabeledBox, delta: Pos[Int]): LabeledBox = value.copy(box = value.box + delta)
    }
  }
  implicit class BoxOps(x: Box[Int]) {
    def named(id: Int): LabeledBox = LabeledBox(x, id)
  }

  def collectIDs(tree: RTree[Int,LabeledBox]): Map[Box[Int],Set[Int]] = {
    val map = new scala.collection.mutable.HashMap[Box[Int],Set[Int]]
    tree.iterate{(_,box,vs) => map(box) = map.getOrElse(box, Set.empty) ++ vs.map(_.id) }
    map.toMap
  }
}
