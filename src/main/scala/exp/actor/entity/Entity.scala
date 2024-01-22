package exp.actor.entity

import common.immutable.{Cube, Pos}
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
  def borders: Iterator[Cube[Long]] = parts.borders.all
  def size: Long = iterator.map(_.volume.size).sum

  def falls: Boolean = iterator.forall(_.material.falls)
  def immortal: Boolean = iterator.exists(_.material.immortal)

  def break(groups: Iterator[Parts]): Iterator[Entity]

  def scale(n: Long): Unit = parts = parts.map{part =>
    val shape = part.volume.shape
    val middle = part.volume.min + shape/2
    val scaled = shape/(2*n)
    val next = Cube(middle - scaled, middle + scaled)
    Part(next, part.material, part.health)
  }

  def above: Iterator[Entity] = parts.borders.up.flatMap{b => world.actors.getExcept(b, this) }
  def bordering: Iterator[Entity] = parts.borders.all.flatMap{b => world.actors.getExcept(b, this) }

  def draw(g: Draw2D): Unit = iterator.foreach(_.draw(g))
  def highlight(g: Draw2D, brighter: Boolean): Unit = iterator.foreach(_.highlight(g, brighter))

  def overlappingParts(rhs: Cube[Long]): Iterator[Part] = iterator.filter(_.volume.overlaps(rhs))
  def overlaps(rhs: Cube[Long]): Boolean = iterator.exists(_.volume.overlaps(rhs))
  def contains(rhs: Pos[Long]): Boolean = iterator.exists(_.volume.contains(rhs))
  def at(rhs: Pos[Long]): Option[Part] = iterator.find(_.volume.contains(rhs))

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
    val remaining = mutable.ArrayBuffer.empty[Part]
    var changed: Boolean = false
    parts.foreach{part => part.volume.intersect(hit.vol) match {
      case Some(both) =>
        changed = true
        neighbors ++= world.actors.getExcept(Cube(part.volume.min - 1, part.volume.max + 1), this)
        remaining ++= (part diff hit.vol)
        if (part.health > hit.strength)
          remaining += Part(both, part.material, part.health - hit.strength)
      case None =>
        remaining += part
    }}
    if (remaining.isEmpty) {
      world.messages.broadcast(new message.Removed(this), neighbors)
      die()
    } else {
      val groups = Entity.group(remaining.iterator)
      if (groups.size > 1) {
        val broken = break(groups.iterator).toArray
        world.actors ++= broken
        broken.foreach{e => world.messages.send(new message.Move(e), e) }
        world.messages.broadcast(new message.Move(this), neighbors) // todo: broken, not move
        die()
      } else if (changed) {
        // Need to send message to self to wake up?
        parts = new Parts(remaining)
        world.messages.send(new message.Move(this), this) // todo: broken, not move
        world.messages.broadcast(new message.Move(this), neighbors) // todo: changed, not move
        true
      } else {
        true
      }
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
  def group(parts: Iterator[Part]): Iterable[Parts] = {
    var count: Int = 0
    val groups: mutable.HashMap[Int, Parts] = mutable.HashMap.empty
    val lookup: mutable.HashMap[Part, Int] = mutable.HashMap.empty

    parts.foreach{part =>
      val sets = mutable.HashSet.empty[Int]
      val parts = mutable.ArrayBuffer[Part](part)
      groups.foreach{case (idx, group) =>
        if (group.borders.all.exists{border =>
          border.overlaps(part.volume) }) {
          sets += idx
          parts ++= group.iterator
        }
      }
      groups(count) = new Parts(parts)
      groups.subtractAll(sets)
      parts.foreach{part => lookup(part) = count }
      count += 1
    }
    groups.values
  }

  def updateVelocity(world: World, entity: Entity): Pos[Long] = {
    Pos(entity.velocity.iterator.zip(entity.accel.iterator).zipWithIndex.map{case ((vInit, a), dim) =>
      val reduce = {(a: Long, b: Long) => if (vInit >= 0) Math.min(a,b) else Math.max(a,b) }
      if (entity.iterator.isEmpty || (vInit == 0 && a == 0)) 0 else entity.iterator.map{part =>
        val current = if (vInit >= 0) part.volume.max(dim) else part.volume.min(dim)
        val v = Math.min(world.terminal, vInit + a)
        val trajectory = part.volume.alter(dim, current, current + v) // travel range in this tick
        world.actors.getPartsExcept(trajectory, entity) match {
          case others if others.nonEmpty =>
            val top = if (v >= 0) others.minBy(_.volume.min(dim)) else others.maxBy(_.volume.max(dim))
            val bound = if (v >= 0) top.volume.min(dim) - 1 else top.volume.max(dim) + 1
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
