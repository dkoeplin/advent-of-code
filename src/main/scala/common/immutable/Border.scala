package common.immutable

import common.traits.RTreeHash

case class Border[A: Numeric](dim: Int, dir: Dir, box: Box[A]) {
  def diff(rhs: Box[A]): Iterator[Border[A]] = (box diff rhs).map { v => Border(dim, dir, v) }
  def diff(rhs: IterableOnce[Box[A]]): Iterator[Border[A]] = (box diff rhs).map { v => Border(dim, dir, v) }

  def thickness: A = box.shape(dim)

  def +(delta: Pos[A]): Border[A] = Border(dim, dir, box + delta)
}

object Border {
  implicit val intHasRTreeHash: RTreeHash[Int, Border[Int]] = new RTreeHash[Int,Border[Int]] {
    override def box(value: Border[Int]): Box[Int] = value.box
    override def hash(value: Border[Int]): Int = value.hashCode()
    override def move(value: Border[Int], delta: Pos[Int]): Border[Int] = value.copy(box = value.box + delta)
  }
  implicit val longHasRTreeHash: RTreeHash[Long, Border[Long]] = new RTreeHash[Long, Border[Long]] {
    override def box(value: Border[Long]): Box[Long] = value.box
    override def hash(value: Border[Long]): Long = value.hashCode().toLong
    override def move(value: Border[Long], delta: Pos[Long]): Border[Long] = value.copy(box = value.box + delta)
  }
}
