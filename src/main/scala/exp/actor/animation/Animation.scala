package exp.actor.animation

import exp.World
import exp.actor.Actor
import exp.actor.entity.Part
import exp.draw.Draw2D

abstract class Animation(id: Actor.ID, world: World, parts: Part.Tree) extends Actor(id, world) {
  override def draw(g: Draw2D): Unit = ???

  override def die(): Unit = { alive = false; world.actors -= this }
}
