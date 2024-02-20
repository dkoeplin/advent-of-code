package common.immutable

import common.traits.RTreeHash

case class View[V](value: V) {
  def at[A](offset: Pos[A])(implicit hash: RTreeHash[A, V]): V = hash.move(value, offset)
}

object View {
  def RTreeHash[A, V](hasher: RTreeHash[A, V]): RTreeHash[A, View[V]] = new RTreeHash[A, View[V]] {
    override def box(view: View[V]): Box[A] = hasher.box(view.value)

    override def hash(view: View[V]): A = hasher.hash(view.value)

    override def move(view: View[V], delta: Pos[A]): View[V] = View(hasher.move(view.value, delta))
  }
}
