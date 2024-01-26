package common.immutable

case class Border[A](dim: Int, dir: Pos[A], volume: Box[A]) {
  def diff(rhs: Box[A]): Iterator[Border[A]] = (volume diff rhs).map{v => Border(dim, dir, v) }
  def diff(rhs: IterableOnce[Box[A]]): Iterator[Border[A]] = (volume diff rhs).map{v => Border(dim, dir, v) }
  def thickness: A = volume.shape(dim)
}
