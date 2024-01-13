package exp

import common.immutable.{Cube, Pos}
import exp.entity.Part

import scala.swing.Graphics2D

abstract class Entity(val id: Int, val world: World, vs: Array[Part]) {
  protected var velocity: Pos[Double] = Pos(0, 0)
  protected var parts: Array[Part] = vs
  def bbox: Cube[Double] = parts.iterator.map(_.volume).reduce(_ union _)
  def size: Double = parts.iterator.map(_.volume.size).sum

  def iterator: Iterator[Part] = parts.iterator

  def scale(n: Double): this.type = {
    parts = parts.map{p =>
      val shape = p.volume.shape
      val next = Cube(p.volume.min + shape*(0.5 - 0.5*n), p.volume.min + shape*(0.5 + 0.5*n))
      Part(next, p.material)
    }
    this
  }

  object State {
    val Awake = 0
    val Asleep = 1
    val Dying = 2
    val Dead = 3
  }

  def falls: Boolean = !dying && parts.forall(_.material.falls)

  def above: Iterator[Entity] = world.entities.overlappingExcept(bbox.above(Entity.kUpdateRange), Some(this))
  def bordering(width: Double = Entity.kUpdateRange): Iterator[Entity]
    = bbox.borders(width).flatMap{border => world.entities.overlappingExcept(border, Some(this)) }

  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case e: Entity => id == e.id
    case _ => false
  }
  def ==(rhs: Entity): Boolean = id == rhs.id
  def !=(rhs: Entity): Boolean = id != rhs.id

  def draw(g: Graphics2D): Unit = parts.foreach(_.draw(g))

  def highlight(g: Graphics2D, brighter: Boolean): Unit = parts.foreach{case Part(volume, material) =>
    val border = volume.roundInt
    g.setColor(if (brighter) material.color.brighter() else material.color.darker())
    g.fillRect(border.min.x, border.min.y, border.shape.x, border.shape.y)
  }

  def overlaps(rhs: Cube[Double]): Boolean = parts.exists(_.volume.overlaps(rhs))
  def contains(rhs: Pos[Double]): Boolean = parts.exists(_.volume.contains(rhs))
  def at(rhs: Pos[Double]): Option[Part] = parts.find(_.volume.contains(rhs))

  def updateVelocity(): Pos[Double] = Pos(velocity.iterator.zipWithIndex.map{case (v, dim) =>
    val p = if (v >= 0) bbox.max(dim) else bbox.min(dim)
    val mayAccel = World.isVertical(dim) && falls
    if (v != 0 || mayAccel) {
      val b = bbox.alter(dim, p, p + v) // travel range in this tick
      world.entities.overlappingExcept(b, Some(this)) match {
        case entities if entities.nonEmpty =>
          val top = if (v >= 0) entities.minBy(_.bbox.min(dim)) else entities.maxBy(_.bbox.max(dim))
          val bound = if (v >= 0) top.bbox.min(dim) else top.bbox.max(dim)
          bound - p
        case _ if World.isVertical(dim) && falls =>
          Math.min(Exp.terminalVelocity, v + Exp.gravity)
        case _ => v
      }
    } else 0
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
  val kDeathTime: Int = 40
  val kUpdateRange: Int = 5
}
