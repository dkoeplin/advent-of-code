package exp.actor.entity

import common.immutable.{Box, View}
import exp.World
import exp.actor.Actor
import exp.draw.Draw2D
import exp.material.Material

class Block(id: Actor.ID, world: World, parts: Part.Tree) extends Entity(id, world, parts) {
  def this(id: Actor.ID, world: World, vol: Box[Long], mat: Material)
  = this(id, world, Part.Tree.one(new Part(vol, mat)))

  val material: Material = parts.iterator.next().material

  override def draw(g: Draw2D): Unit = g.at(loc) {
    parts.view.iterator.foreach { case View(part) => part.draw(g) }
    parts.view.borders().foreach { case View(border) => g.fillRect(border.box, material.color.darker()) }
  }

  override def highlight(g: Draw2D, brighter: Boolean): Unit = g.at(loc) {
    parts.view.iterator.foreach { case View(part) => part.highlight(g, brighter) }
    parts.view.borders().foreach { case View(border) => g.fillRect(border.box, material.color.brighter()) }
  }

  override def break(groups: Iterable[Part.Tree]): Iterable[Entity]
  = groups.map { group => new Block(world.actors.nextId, world, group) }

  override def toString: String = s"Block(#$id)"
}