package common.traits

import common.immutable.Box

trait RTreeHash[A,V] {
  def box(value: V): Box[A]
  def hash(value: V): A
}
object RTreeHash {
  implicit class VolumeOps[A,V](x: V)(implicit has: RTreeHash[A,V]) {
    def box: Box[A] = has.box(x)
    def hash: A = has.hash(x)
  }
}
