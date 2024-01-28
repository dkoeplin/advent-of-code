package exp.actor.entity

import common.immutable.{Box, Pos}
import exp.actor.Actor
import exp.draw.Draw2D
import exp.message.Message
import exp.{World, message}

import scala.collection.mutable

abstract class Entity(id: Actor.ID, world: World, _parts: Parts) extends Actor(id, world) {
  protected var parts: Parts = _parts
  protected var velocity: Pos[Long] = Pos.zero[Long](2)
  protected var accel: Pos[Long] = Pos(0, if (falls) world.gravity else 0)

  def num: Int = parts.size
  def iterator: Iterator[Part] = parts.iterator
  def borders: Iterator[Box[Long]] = parts.bounds.all
  def size: Long = iterator.map(_.box.size).sum

  def falls: Boolean = iterator.forall(_.material.falls)
  def immortal: Boolean = iterator.exists(_.material.immortal)

  def break(groups: Iterator[Parts]): Iterator[Entity]

  def scale(n: Long): Unit = parts = parts.map{part =>
    val shape = part.box.shape
    val middle = part.box.min + shape/2
    val scaled = shape/(2*n)
    val next = Box(middle - scaled, middle + scaled)
    Part(next, part.material, part.health)
  }

  def above: Iterator[Entity] = parts.bounds.up.flatMap{ b => world.actors.getExcept(b, this) }
  def bordering: Iterator[Entity] = parts.bounds.all.flatMap{ b => world.actors.getExcept(b, this) }

  def draw(g: Draw2D): Unit = iterator.foreach(_.draw(g))
  def highlight(g: Draw2D, brighter: Boolean): Unit = iterator.foreach(_.highlight(g, brighter))

  def overlappingParts(rhs: Box[Long]): Iterator[Part] = iterator.filter(_.box.overlaps(rhs))
  def overlaps(rhs: Box[Long]): Boolean = iterator.exists(_.box.overlaps(rhs))
  def contains(rhs: Pos[Long]): Boolean = iterator.exists(_.box.contains(rhs))
  def at(rhs: Pos[Long]): Option[Part] = iterator.find(_.box.contains(rhs))

  override def tick(): Unit = {
    if (!receiveAll()) // Get all messages first
      return // End early if we died

    val initialVelocity = velocity
    velocity = Entity.updateVelocity(world, this)

    // Check for and notify neighbors before moving
    if (velocity.magnitude == 0) {
      sleep()
    } else if (initialVelocity.magnitude == 0) {
      world.messages.broadcast(new message.Move(this), to=above)
    }

    parts = parts.map(_ + velocity)
  }

  final protected def receiveAll(): Boolean = world.messages.get(this).forall(receive)
  protected def receive(m: Message): Boolean = m match {
    case _: message.Delete => remove()
    case h: message.Hit => hit(h)
    case _ => true
  }

  protected def hit(hit: message.Hit): Boolean = {
    val neighbors = mutable.ArrayBuffer.empty[Entity]
    val groups = PartsGroupBuilder.empty
    // val remaining = mutable.ArrayBuffer.empty[Part]
    var changed: Boolean = false
    parts.foreach{part => part.box.intersect(hit.vol) match {
      case Some(both) =>
        changed = true
        neighbors ++= world.actors.getExcept(Box(part.box.min - 1, part.box.max + 1), this)
        groups ++= (part diff hit.vol)
        if (part.health > hit.strength)
          groups += Part(both, part.material, part.health - hit.strength)
      case None =>
        groups += part
    }}
    if (groups.isEmpty) {
      world.messages.broadcast(new message.Removed(this), neighbors)
      die()
    } else if (groups.size > 1) {
      val broken = break(groups.finish).toArray
      world.actors ++= broken
      broken.foreach{e => world.messages.send(new message.Move(e), e) }
      world.messages.broadcast(new message.Move(this), neighbors) // todo: broken, not move
      die()
    } else if (changed) {
      // Need to send message to self to wake up?
      parts = groups.finish.next()
      world.messages.send(new message.Move(this), this) // todo: broken, not move
      world.messages.broadcast(new message.Move(this), neighbors) // todo: changed, not move
      true
    } else {
      true
    }
  }

  protected def remove(): Boolean = if (immortal) true else {
    world.messages.broadcast(new message.Removed(this), to=bordering)
    die()
  }

  final def wake(): Unit = { world.actors.wake(this) }
  final protected def die(): Boolean = {
    world.actors -= this
    false
  }
  final protected def sleep(): Unit = { world.actors.sleep(this) }
}
object Entity {
  def updateVelocity(world: World, entity: Entity): Pos[Long] = {
    Pos(entity.velocity.iterator.zip(entity.accel.iterator).zipWithIndex.map{case ((vInit, a), dim) =>
      val reduce = {(a: Long, b: Long) => if (vInit >= 0) Math.min(a,b) else Math.max(a,b) }
      if (entity.iterator.isEmpty || (vInit == 0 && a == 0)) 0 else entity.iterator.map{part =>
        val current = if (vInit >= 0) part.box.max(dim) else part.box.min(dim)
        val v = Math.min(world.terminal, vInit + a)
        val trajectory = part.box.alter(dim, current, current + v) // travel range in this tick
        world.actors.getPartsExcept(trajectory, entity) match {
          case others if others.nonEmpty =>
            val top = if (v >= 0) others.minBy(_.box.min(dim)) else others.maxBy(_.box.max(dim))
            val bound = if (v >= 0) top.box.min(dim) - 1 else top.box.max(dim) + 1
            bound - current
          case _ => v
        }
      }.reduce(reduce)
    })
  }

  val kDeathRate: Long = 2     // Shrink factor per tick while dying
  val kDeathTime: Int = 40     // Number of ticks to spend in the "dying" animation
  val kUpdateRange: Int = 2    // Number of pixels range to update on change by default
}
