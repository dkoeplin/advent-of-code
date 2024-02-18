package exp.actor

import common.immutable.{Box, Pos}
import common.mutable.RTree
import exp.actor.animation.Animation
import exp.actor.entity.{Entity, Part}

import scala.collection.mutable

/**
 * Manages all active actors.
 *            Interaction   Awake  Visible
 * Entity          Y         Y/N      Y
 * Animation       N          Y       Y
 */
class Manager {
  private var id: Actor.ID = 0
  // Entities - can be interacted with
  // private val entityList: mutable.LinkedHashSet[Entity] = mutable.LinkedHashSet.empty
  private val entityTree: RTree[Long, Entity] = RTree.empty[Long, Entity](2)
  // Awake - active tick()
  private val awakeList: mutable.LinkedHashSet[Actor] = mutable.LinkedHashSet.empty
  // Visible animations
  private val animations: RTree[Long, Actor] = RTree.empty[Long, Actor](2)

  val killed: mutable.ArrayBuffer[Long] = mutable.ArrayBuffer.empty

  // DEBUG
  def tree: RTree[Long, Entity] = entityTree

  def entities: Iterator[Entity] = entityTree.iterator
  def visible(area: Box[Long]): Iterable[Actor] = entityTree(area) ++ animations(area)
  def awake: Iterator[Actor] = awakeList.iterator

  def nextId: Actor.ID = { id += 1; id }

  def -=(e: Entity): Unit = { entityTree -= e; awakeList.remove(e); killed += e.id }
  def +=(e: Entity): Unit = if (e.isAlive) { entityTree += e; awakeList.add(e) }
  def ++=(es: IterableOnce[Entity]): Unit = es.iterator.filter(_.isAlive).foreach{e => this += e }

  def -=(a: Animation): Unit = { awakeList.remove(a); animations -= a }
  def +=(a: Animation): Unit = if (a.isAlive) { awakeList.add(a); animations += a }

  def near(x: Box[Long]): Iterator[Entity] = entityTree(x).iterator
  def nearExcept(x: Box[Long], e: Entity): Iterator[Entity] = near(x).filterNot(_ == e)

  def find(x: Pos[Long]): Option[Entity] = near(Box(x,x)).find(_.contains(x))

  def get(volume: Box[Long]): Iterator[Entity] = near(volume).filter(_.overlaps(volume))
  def getExcept(volume: Box[Long], e: Entity): Iterator[Entity] = nearExcept(volume, e).filter(_.overlaps(volume))

  def getParts(volume: Box[Long]): Iterator[Part] = near(volume).flatMap(_.overlappingParts(volume))
  def getPartsExcept(volume: Box[Long], e: Entity): Iterator[Part] = nearExcept(volume, e).flatMap(_.overlappingParts(volume))

  def getEntity(id: Actor.ID): Option[Entity] = entityTree.get(id)
  def wake(e: Entity): Unit = if (e.isAlive) { awakeList += e }
  def sleep(e: Entity): Unit = { awakeList -= e }
  def moved(e: Entity, prev: Box[Long]): Unit = if (e.isAlive) { entityTree.moveEntry(e, prev) }
}
