package exp.entity

import common.immutable.{Cube, Pos}
import exp.draw.Draw2D
import exp.message.Message
import exp.{World, message}

import scala.collection.mutable

abstract class Entity(val id: Int, val world: World, _parts: Parts) {
  object State {
    val Awake = 0
    val Asleep = 1
    val Dying = 2
    val Dead = 3
  }

  protected var parts: Parts = _parts
  private var state: Int = State.Awake
  private var death: Int = 0

  protected var velocity: Pos[Long] = Pos.zero[Long](2)
  protected var accel: Pos[Long] = Pos(0, if (falls) world.gravity else 0)

  def iterator: Iterator[Part] = parts.iterator
  def borders: Iterator[Cube[Long]] = parts.borders.all
  def size: Long = iterator.map(_.volume.size).sum

  def falls: Boolean = !dying && iterator.forall(_.material.falls)
  def awake: Boolean = state == State.Awake
  def asleep: Boolean = state == State.Asleep
  def dying: Boolean = state == State.Dying
  def dead: Boolean = state == State.Dead
  def alive: Boolean = { state != State.Dying && state != State.Dead }
  def immortal: Boolean = iterator.exists(_.material.immortal)

  def break(groups: Iterator[Parts]): Iterator[Entity]

  def scale(n: Long): Unit = parts = parts.map{part =>
    val shape = part.volume.shape
    val middle = part.volume.min + shape/2
    val scaled = shape/(2*n)
    val next = Cube(middle - scaled, middle + scaled)
    Part(next, part.material)
  }

  def above: Iterator[Entity] = parts.borders.up.flatMap{b => world.entities.overlappingExcept(b, Some(this)) }
  def bordering: Iterator[Entity] = parts.borders.all.flatMap{b => world.entities.overlappingExcept(b, Some(this)) }

  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case e: Entity => id == e.id
    case _ => false
  }
  def ==(rhs: Entity): Boolean = id == rhs.id
  def !=(rhs: Entity): Boolean = id != rhs.id

  def draw(g: Draw2D): Unit = iterator.foreach(_.draw(g))

  def highlight(g: Draw2D, brighter: Boolean): Unit = iterator.foreach{case Part(volume, material) =>
    g.fillRect(volume, if (brighter) material.color.brighter() else material.color.darker())
  }

  def overlappingParts(rhs: Cube[Long]): Iterator[Part] = iterator.filter(_.volume.overlaps(rhs))
  def overlaps(rhs: Cube[Long]): Boolean = iterator.exists(_.volume.overlaps(rhs))
  def contains(rhs: Pos[Long]): Boolean = iterator.exists(_.volume.contains(rhs))
  def at(rhs: Pos[Long]): Option[Part] = iterator.find(_.volume.contains(rhs))

  def remove(rhs: Cube[Long]): Unit = if (alive) {
    val neighbors = mutable.ArrayBuffer.empty[Entity]
    val remaining = mutable.ArrayBuffer.empty[Part]
    var changed: Boolean = false
    parts.foreach{part =>
      if (part.volume.overlaps(rhs)) {
        changed = true
        neighbors ++= world.entities.overlappingExcept(Cube(part.volume.min - 1, part.volume.max + 1), Some(this))
        remaining ++= (part diff rhs)
      } else {
        remaining += part
      }
    }
    if (remaining.isEmpty) {
      die()
      world.messages.broadcast(new message.Removed(this), neighbors)
    } else {
      val groups = Entity.group(remaining.iterator)
      if (groups.size > 1) {
        die()
        val broken = break(groups.iterator).toArray
        world.entities ++= broken
        broken.foreach{e => world.messages.send(new message.Move(e), e) }
      } else if (changed) {
        // Need to send message to self to wake up?
        parts = new Parts(remaining)
        world.messages.send(new message.Move(this), this) // todo: broken, not move
      }
      if (changed) {
        world.messages.broadcast(new message.Move(this), neighbors) // todo: changed, not move
      }
    }
  }

  def tick(): Unit = {
    val initialVelocity = velocity
    velocity = Entity.updateVelocity(world, this)

    // Check for and notify neighbors before moving
    if (dying) {
      death += 1
      scale(Entity.kDeathRate)
      if (death > Entity.kDeathTime) die()
    } else if (velocity.magnitude == 0) {
      sleep()
    } else if (initialVelocity.magnitude == 0) {
      world.messages.broadcast(new message.Move(this), to=above)
    }

    parts = parts.map(_ + velocity)
  }

  protected def receive(m: Message): Unit = {}
  final protected def receiveAll(): Unit = world.messages.get(this).foreach(receive)

  final protected def die(): Unit = {
    state = State.Dead
    world.entities.notifyDead(this)
  }
  final def kill(): Unit = if (!immortal) {
    state = State.Dying
    world.entities.notifyAwake(this)
    world.messages.broadcast(new message.Removed(this), to=bordering)
  }
  final def wake(): Unit = {
    state = State.Awake
    world.entities.notifyAwake(this)
  }
  final protected def sleep(): Unit = {
    state = State.Asleep
    world.entities.notifySleep(this)
  }
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
        world.entities.overlappingPartsExcept(trajectory, Some(entity)) match {
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
