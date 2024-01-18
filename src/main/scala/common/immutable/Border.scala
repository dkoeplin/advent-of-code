package common.immutable

case class Border[A](dim: Int, dir: Pos[A], volume: Cube[A]) {
  def diff(rhs: Cube[A]): Set[Border[A]] = (volume diff rhs).map{v => Border(dim, dir, v) }
  def thickness: A = volume.shape(dim)
}
