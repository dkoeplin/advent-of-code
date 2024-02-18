package exp.actor.animation

import exp.World
import exp.actor.Actor
import exp.actor.entity.Parts
import exp.draw.Draw2D

abstract class Animation(id: Actor.ID, world: World, var parts: Parts) extends Actor(id, world) {
  override def draw(g: Draw2D): Unit = parts.draw(g)

  override def die(): Unit = { alive = false; world.actors -= this }
}
