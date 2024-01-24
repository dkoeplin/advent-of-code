package exp.actor

import common.immutable.{Box, Pos}
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
  private val entityList: mutable.LinkedHashSet[Entity] = mutable.LinkedHashSet.empty
  // Awake - active tick()
  private val awakeList: mutable.LinkedHashSet[Actor] = mutable.LinkedHashSet.empty
  // Visible animations
  private val animationList: mutable.LinkedHashSet[Animation] = mutable.LinkedHashSet.empty

  def entities: Iterator[Entity] = entityList.iterator
  def visible: Iterator[Actor] = entityList.iterator ++ animationList.iterator
  def awake: Iterator[Actor] = awakeList.iterator

  def nextId: Actor.ID = { id += 1; id }

  def -=(e: Entity): Unit = { entityList.remove(e); awakeList.remove(e) }
  def +=(e: Entity): Unit = { entityList.add(e); awakeList.add(e) }
  def ++=(e: IterableOnce[Entity]): Unit = { entityList.addAll(e); awakeList.addAll(e) }

  def -=(a: Animation): Unit = { awakeList.remove(a); animationList.remove(a) }
  def +=(a: Animation): Unit = { awakeList.add(a); animationList.add(a) }

  // TODO: Limit this somehow
  def near(x: Box[Long]): Iterator[Entity] = entityList.iterator
  def nearExcept(x: Box[Long], e: Entity): Iterator[Entity] = near(x).filterNot(_ == e)

  def find(x: Pos[Long]): Option[Entity] = near(Box(x,x)).find(_.contains(x))

  def get(volume: Box[Long]): Iterator[Entity] = near(volume).filter(_.overlaps(volume))
  def getExcept(volume: Box[Long], e: Entity): Iterator[Entity] = nearExcept(volume, e).filter(_.overlaps(volume))

  def getParts(volume: Box[Long]): Iterator[Part] = near(volume).flatMap(_.overlappingParts(volume))
  def getPartsExcept(volume: Box[Long], e: Entity): Iterator[Part] = nearExcept(volume, e).flatMap(_.overlappingParts(volume))

  def wake(e: Entity): Unit = { awakeList += e }
  def sleep(e: Entity): Unit = { awakeList -= e }
}
