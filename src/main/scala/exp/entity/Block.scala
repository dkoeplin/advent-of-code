package exp.entity

import common.immutable.Cube
import exp.World
import exp.material.Material

import scala.collection.immutable.ArraySeq

class Block(id: Int, world: World, parts: Parts) extends Entity(id, world, parts) {
  def this(id: Int, world: World, vol: Cube[Long], mat: Material) = this(id, world, new Parts(ArraySeq(Part(vol, mat))))
  val material: Material = parts.iterator.next().material
  override def toString: String = s"Block(#$id)"

  override def break(groups: Iterator[Parts]): Iterator[Entity] = groups.map{group =>
    new Block(world.entities.nextId, world, group)
  }
}