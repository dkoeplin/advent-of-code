package exp.actor.entity

import common.immutable.Cube
import exp.World
import exp.actor.Actor
import exp.material.Material

class Block(id: Actor.ID, world: World, parts: Parts) extends Entity(id, world, parts) {
  def this(id: Actor.ID, world: World, vol: Cube[Long], mat: Material) = this(id, world, new Parts(vol, mat))
  val material: Material = parts.iterator.next().material

  override def break(groups: Iterator[Parts]): Iterator[Entity] = groups.map{group =>
    new Block(world.actors.nextId, world, group)
  }

  override def toString: String = s"Block(#$id)"
}