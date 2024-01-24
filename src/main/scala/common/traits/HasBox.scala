package common.traits

import common.immutable.Box

trait HasBox[A,V] {
  def box(value: V): Box[A]
}
object HasBox {
  implicit class VolumeOps[A,V](x: V)(implicit has: HasBox[A,V]) {
    def box: Box[A] = has.box(x)
  }
}
