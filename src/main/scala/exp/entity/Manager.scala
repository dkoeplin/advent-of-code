package exp.entity

import common.immutable.{Cube, Pos}

import scala.collection.mutable

class Manager {
  private var id: Int = 0
  private val allEntities: mutable.LinkedHashSet[Entity] = mutable.LinkedHashSet.empty
  private val awakeEntities: mutable.LinkedHashSet[Entity] = mutable.LinkedHashSet.empty

  def nextId: Int = { id += 1; id }

  def all: Iterator[Entity] = allEntities.iterator
  def alive: Iterator[Entity] = allEntities.iterator.filter(_.alive)
  def awake: Iterator[Entity] = awakeEntities.iterator

  def +=(e: Entity): Unit = {
    allEntities += e
    awakeEntities += e
  }
  def -=(e: Entity): Unit = {
    allEntities -= e
    awakeEntities -= e
  }
  def ++=(e: IterableOnce[Entity]): Unit = {
    allEntities ++= e
    awakeEntities ++= e
  }

  // TODO: Limit this somehow
  def nearbyExcept(x: Cube[Long], target: Option[Entity]): Iterator[Entity] = alive.iterator.filterNot(target.contains)

  def find(x: Pos[Long]): Option[Entity] = nearbyExcept(Cube(x,x), None).find{e => e.contains(x) && e.alive }

  def overlappingExcept(volume: Cube[Long], target: Option[Entity]): Iterator[Entity] = {
    nearbyExcept(volume, target).filter{e => e.overlaps(volume) && e.alive }
  }
  def overlappingPartsExcept(volume: Cube[Long], target: Option[Entity]): Iterator[Part] = {
    nearbyExcept(volume, target).flatMap{e => e.overlappingParts(volume) }
  }

  def notifyAwake(e: Entity): Unit = { awakeEntities += e }
  def notifySleep(e: Entity): Unit = { awakeEntities -= e }
  def notifyDead(e: Entity): Unit = { awakeEntities -= e; allEntities -= e }
}
