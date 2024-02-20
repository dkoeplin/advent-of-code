package exp.actor

import common.immutable.{Box, Pos}
import common.traits.RTreeHash
import exp.World
import exp.draw.Draw2D

abstract class Actor(val id: Actor.ID, val world: World) {
  protected var alive: Boolean = true

  def isAlive: Boolean = alive
  def bbox: Box[Long]

  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case e: Actor => id == e.id
    case _ => false
  }
  def ==(rhs: Actor): Boolean = id == rhs.id
  def !=(rhs: Actor): Boolean = id != rhs.id

  def tick(): Unit
  def draw(g: Draw2D): Unit

  protected def die(): Unit
}
object Actor {
  type ID = Long
  implicit def actorHasRTreeHash[A<:Actor]: RTreeHash[Long, A] = new RTreeHash[Long,A] {
    override def hash(a: A): Long = a.id
    override def box(a: A): Box[ID] = a.bbox

    override def move(value: A, delta: Pos[ID]): A = throw new Exception("Cannot copy actors")
  }
}
