package exp.entity

import common.immutable.Cube
import exp.{Entity, Material, World}
import exp.message

class Block(id: Int, world: World, vol: Cube[Double], mat: Material)
  extends Entity(id, world, Array(Part(vol, mat))) {
  val material: Material = mat
  override def toString: String = s"Block(#$id, $vol)"
}