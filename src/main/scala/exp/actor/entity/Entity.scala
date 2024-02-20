package exp.actor.entity

import common.immutable.{Border, Box, Pos, View}
import common.mutable.BorderedRTree
import exp.actor.Actor
import exp.draw.Draw2D
import exp.message.Message
import exp.{World, message}

import scala.collection.mutable

abstract class Entity(id: Actor.ID, world: World, parts: Part.Tree) extends Actor(id, world) {
  protected var velocity: Pos[Long] = Pos.zero[Long](2)
  protected var accel: Pos[Long] = Pos(0, if (falls) world.gravity else 0)

  def break(groups: Iterable[Part.Tree]): Iterable[Entity]

  def loc: Pos[Long] = parts.loc
  def size: Long = iterator.map(_.box.size).sum
  def falls: Boolean = iterator.forall(_.material.falls)
  def immortal: Boolean = iterator.forall(_.material.immortal)
  def bbox: Box[Long] = parts.bbox
  def tree: Part.Tree = parts

  def iterator: Iterator[Part] = parts.iterator

  def borders(): Iterator[Border[Long]] = parts.borders()
  def borders(predicate: Border[Long] => Boolean): Iterator[Border[Long]] = parts.borders(predicate)

  def bounds(): Iterator[Box[Long]] = parts.borders().map(_.box)
  def bounds(predicate: Border[Long] => Boolean): Iterator[Box[Long]] = parts.borders(predicate).map(_.box)

  def move(delta: Pos[Long]): Unit = moveto(loc + delta)
  def moveto(pos: Pos[Long]): Unit = {
    val prev = bbox
    parts.moveto(pos)
    world.actors.moved(this, prev)
  }

  def above: Iterator[Entity] = bounds(World.Up).flatMap { b => world.actors.getExcept(b, this) }

  def bordering: Iterator[Entity] = bounds().flatMap { b => world.actors.getExcept(b, this) }

  def draw(g: Draw2D): Unit = g.at(loc) {
    parts.view.iterator.foreach { case View(part) => part.draw(g) }
  }

  def highlight(g: Draw2D, brighter: Boolean): Unit = g.at(loc) {
    parts.view.iterator.foreach { case View(part) => part.highlight(g, brighter) }
  }

  def overlappingParts(rhs: Box[Long]): Iterator[Part] = iterator.filter(_.box.overlaps(rhs))
  def overlaps(rhs: Box[Long]): Boolean = iterator.exists(_.box.overlaps(rhs))
  def contains(rhs: Pos[Long]): Boolean = iterator.exists(_.box.contains(rhs))
  def at(rhs: Pos[Long]): Option[Part] = iterator.find(_.box.contains(rhs))

  override def tick(): Unit = if (isAlive) {
    val nextStatus = receiveAll()    // Get all messages first
    if (nextStatus == Status.Dead) { // End early if died
      die()
    } else {
      val initialVelocity = velocity
      velocity = Entity.updateVelocity(world, this)

      // Check for and notify neighbors before moving
      if (bbox.max.apply(1) > 10000) {
        die()
      } else if (velocity.magnitude == 0) {
        if (nextStatus != Status.Wake) sleep()
      } else if (initialVelocity.magnitude == 0) { // Notify blocks above that we're moving
        world.messages.broadcast(new message.Move(this), to=above)
      }
      move(velocity)
    }
  }

  final protected def receiveAll(): Status = {
    val neighbors = mutable.LinkedHashSet.empty[Entity]
    var nextStatus: Status = Status.None
    var messages = world.messages.get(this)
    while (nextStatus != Status.Dead && messages.nonEmpty) {
      nextStatus |= receive(messages.head, neighbors)
      messages = messages.tail
    }
    if (nextStatus == Status.Hits) {
      val groups = parts.view.components()
      if (groups.size > 1) {
        world.messages.broadcast(new message.Move(this), neighbors) // todo: broken, not move
        // Broken - need to update
        val broken = break(groups.map { group => BorderedRTree(parts.rank, loc, group) })
        world.actors ++= broken
        broken.foreach { e => world.messages.send(new message.Move(e), e) }
        nextStatus = Status.Dead
      } else {
        // Not broken - just broadcast to neighbors
        world.messages.broadcast(new message.Move(this), neighbors) // todo: changed, not move
        nextStatus = Status.Wake
      }
    }
    nextStatus
  }

  protected def receive(m: Message, neighbors: mutable.LinkedHashSet[Entity]): Status = m match {
    case _: message.Delete => remove()
    case h: message.Hit => hit(h, neighbors)
    case _ => Status.None
  }

  protected def hit(hit: message.Hit, neighbors: mutable.LinkedHashSet[Entity]): Status = {
    // val groups = Parts.Grouping.empty
    var changed: Boolean = false
    val relative = hit.box - parts.loc
    parts.view.apply(relative).foreach { case view@View(part) =>
      changed = true
      val area = Box(part.box.min + parts.loc - 1, part.box.max + parts.loc + 1)
      neighbors ++= world.actors.getExcept(area, this)
      parts.view -= view
      parts.view ++= (part diff relative).map(View.apply)
      if (part.health > hit.strength)
        parts.view += View(new Part((part.box intersect relative).get, part.material, part.health - hit.strength))
    }
    if (changed) Status.Hits else Status.None
  }

  protected def remove(): Status = if (immortal) Status.None else {
    world.messages.broadcast(new message.Removed(this), to=bordering)
    Status.Dead
  }

  final def wake(): Unit = { world.actors.wake(this) }
  final protected def die(): Unit = {
    alive = false
    world.actors -= this
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
