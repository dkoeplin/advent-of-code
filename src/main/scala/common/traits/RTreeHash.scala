package common.traits

import common.immutable.{Box, Pos}

trait RTreeHash[A,V] {
  def box(value: V): Box[A]
  def hash(value: V): A

  def move(value: V, delta: Pos[A]): V
}
object RTreeHash {
  implicit class ValueOps[A, V](x: V)(implicit has: RTreeHash[A, V]) {
    def box: Box[A] = has.box(x)
    def hash: A = has.hash(x)
    def move(delta: Pos[A]): V = has.move(x, delta)
  }
}
