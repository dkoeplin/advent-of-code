package exp.entity

import common.immutable.{Cube, Pos}
import exp.draw.Draw2D
import exp.message.Message
import exp.{Exp, World, message}

abstract class Entity(val id: Int, val world: World, vs: Array[Part]) {
  protected var velocity: Pos[Double] = Pos(0, 0)
  protected var parts: Array[Part] = vs
  def size: Double = parts.iterator.map(_.volume.size).sum
  def iterator: Iterator[Part] = parts.iterator

  def scale(n: Double): Unit = parts = parts.map{p =>
    val shape = p.volume.shape
    val next = Cube(p.volume.min + shape*(0.5 - 0.5*n), p.volume.min + shape*(0.5 + 0.5*n))
    Part(next, p.material)
  }

  object State {
    val Awake = 0
    val Asleep = 1
    val Dying = 2
    val Dead = 3
  }

  def falls: Boolean = !dying && parts.forall(_.material.falls)

  def above: Iterator[Entity] = parts.iterator.flatMap{part =>
    world.entities.overlappingExcept(part.volume.above(Entity.kUpdateRange), Some(this))
  }
  def bordering(width: Double = Entity.kUpdateRange): Iterator[Entity] = parts.iterator.flatMap{part =>
    part.volume.borders(width).flatMap{b => world.entities.overlappingExcept(b, Some(this)) }
  }
  // TODO: O(N^2): Memoize or figure out how to make faster
  def borders(width: Double = 1): Iterator[Cube[Double]] = parts.iterator.flatMap{part =>
    part.volume.borders(width).filterNot{b => parts.exists{p => p != part && p.volume.overlaps(b) }}
  }

  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case e: Entity => id == e.id
    case _ => false
  }
  def ==(rhs: Entity): Boolean = id == rhs.id
  def !=(rhs: Entity): Boolean = id != rhs.id

  def draw(g: Draw2D): Unit = parts.foreach(_.draw(g))

  def highlight(g: Draw2D, brighter: Boolean): Unit = parts.foreach{case Part(volume, material) =>
    g.fillRect(volume, if (brighter) material.color.brighter() else material.color.darker())
  }

  def overlappingParts(rhs: Cube[Double]): Iterator[Part] = parts.iterator.filter(_.volume.overlaps(rhs))
  def overlaps(rhs: Cube[Double]): Boolean = parts.exists(_.volume.overlaps(rhs))
  def contains(rhs: Pos[Double]): Boolean = parts.exists(_.volume.contains(rhs))
  def at(rhs: Pos[Double]): Option[Part] = parts.find(_.volume.contains(rhs))

  def remove(rhs: Cube[Double]): Unit = if (alive) {
    val nearby = bordering() // Get the nearby entities _before_ we remove the piece
    parts = parts.flatMap{case Part(volume, material) => (volume diff rhs).map{v => Part(v, material) }}
    if (parts.isEmpty) {
      die()
      world.messages.broadcast(new message.Removed(this), nearby)
    } else {
      world.messages.broadcast(new message.Move(this), nearby) // todo: changed, not move
    }
  }

  def updateVelocity(): Pos[Double] = Pos(velocity.iterator.zipWithIndex.map{case (v, dim) =>
    val mayAccel = World.isVertical(dim) && falls
    val reduce = {(a: Double, b: Double) => if (v > 0) Math.min(a,b) else Math.max(a,b)}
    if (parts.isEmpty || (v == 0 && !mayAccel)) 0.0 else parts.iterator.map{part =>
      val p = if (v >= 0) part.volume.max(dim) else part.volume.min(dim)
      val b = part.volume.alter(dim, p, p + v) // travel range in this tick
      world.entities.overlappingPartsExcept(b, Some(this)) match {
        case others if others.nonEmpty =>
          val top = if (v >= 0) others.minBy(_.volume.min(dim)) else others.maxBy(_.volume.max(dim))
          val bound = if (v >= 0) top.volume.min(dim) else top.volume.max(dim)
          bound - p
        case _ if World.isVertical(dim) && falls =>
          Math.min(Exp.terminalVelocity, v + Exp.gravity)
        case _ => v
      }
    }.reduce(reduce)
  })

  def tick(): Unit = {
    val initialVelocity = velocity
    velocity = updateVelocity()
    parts = parts.map(_ + velocity)

    if (dying) {
      death += 1
      scale(Entity.kDeathRate)
      if (death > Entity.kDeathTime) die()
    } else if (velocity.magnitude == 0) {
      sleep()
    } else if (initialVelocity.magnitude == 0) {
      world.messages.broadcast(new message.Move(this), to=above)
    }
  }

  def awake: Boolean = state == State.Awake
  def asleep: Boolean = state == State.Asleep
  def dying: Boolean = state == State.Dying
  def dead: Boolean = state == State.Dead
  def alive: Boolean = { state != State.Dying && state != State.Dead }
  def immortal: Boolean = parts.exists(_.material.immortal)

  def receive(m: Message): Unit = {}
  final def receiveAll(): Unit = {
    var m: Option[Message] = world.messages.get(this)
    while (m.nonEmpty) {
      receive(m.get)
      m = world.messages.get(this)
    }
  }

  final def die(): Unit = {
    state = State.Dead
    world.entities.notifyDead(this)
  }
  final def kill(): Unit = if (!immortal) {
    state = State.Dying
    world.entities.notifyAwake(this)
    world.messages.broadcast(new message.Removed(this), to=bordering())
  }
  final def wake(): Unit = {
    state = State.Awake
    world.entities.notifyAwake(this)
  }
  final protected def sleep(): Unit = {
    state = State.Asleep
    world.entities.notifySleep(this)
  }
  final private var state: Int = State.Awake
  final private var death: Int = 0
}
object Entity {
  val kDeathRate: Double = 0.9 // Shrink factor per tick while dying
  val kDeathTime: Int = 40     // Number of ticks to spend in the "dying" animation
  val kUpdateRange: Int = 5    // Number of pixels range to update on change by default
}