package exp.entity

import common.immutable.Cube
import exp.{Entity, Material, World}

class Block(id: Int, world: World, vol: Cube[Double], mat: Material)
  extends Entity(id, world, Array(Part(vol, mat))) {
  def first: Part = parts.head
  val material: Material = mat
}