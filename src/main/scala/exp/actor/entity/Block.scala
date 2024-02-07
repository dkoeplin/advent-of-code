package exp.actor.entity

import common.immutable.Box
import exp.World
import exp.actor.Actor
import exp.material.Material

class Block(id: Actor.ID, world: World, _parts: Parts) extends Entity(id, world, _parts) {
  def this(id: Actor.ID, world: World, vol: Box[Long], mat: Material)
    = this(id, world, Parts.single(vol, mat))

  val material: Material = parts.iterator.next().material

  override def break(groups: Iterable[Parts]): Iterable[Entity] = groups.map{group =>
    new Block(world.actors.nextId, world, group)
  }

  override def toString: String = s"Block(#$id)"
}