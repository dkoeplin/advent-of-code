package exp.actor

import exp.World
import exp.draw.Draw2D

abstract class Actor(val id: Actor.ID, val world: World) {
  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case e: Actor => id == e.id
    case _ => false
  }
  def ==(rhs: Actor): Boolean = id == rhs.id
  def !=(rhs: Actor): Boolean = id != rhs.id

  def tick(): Unit
  def draw(g: Draw2D): Unit

  protected def die(): Boolean
}
object Actor {
  type ID = Long
}