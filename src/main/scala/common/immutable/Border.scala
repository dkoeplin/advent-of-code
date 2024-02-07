package common.immutable

case class Border[A:Numeric](dim: Int, dir: Pos[A], volume: Box[A]) {
  def diff(rhs: Box[A]): Iterator[Border[A]] = (volume diff rhs).map{v => Border(dim, dir, v) }
  def diff(rhs: IterableOnce[Box[A]]): Iterator[Border[A]] = (volume diff rhs).map{v => Border(dim, dir, v) }
  def thickness: A = volume.shape(dim)

  def +(delta: Pos[A]): Border[A] = Border(dim, dir, volume + delta)
}
