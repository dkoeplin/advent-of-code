package exp.actor.entity

import common.immutable.Box
import exp.material.Material

case class Part(box: Box[Long], material: Material, health: Int) {
  def overlaps(rhs: Box[Long]): Boolean = box.overlaps(rhs)
  def diff(rhs: Box[Long]): Iterator[Part] = (box diff rhs).map{v => Part(v, material, health) }

}
