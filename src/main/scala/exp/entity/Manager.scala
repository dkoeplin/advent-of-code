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
  def nearby(x: Cube[Double]): Iterator[Entity] = alive.iterator

  def find(x: Pos[Double]): Option[Entity] = {
    nearby(Cube(x,x)).find{e => e.contains(x) && e.alive }
  }
  def findVolume(x: Pos[Int]): Option[Part] = {
    val p = x.toDoubles
    nearby(Cube(p,p)).iterator.find(_.contains(p)).flatMap(_.at(p))
  }

  def overlappingExcept(volume: Cube[Double], target: Option[Entity]): Iterator[Entity] = {
    nearby(volume).filter{e => !target.contains(e) && e.overlaps(volume) && e.alive }
  }
  def overlappingPartsExcept(volume: Cube[Double], target: Option[Entity]): Iterator[Part] = {
    nearby(volume).filter{e => !target.contains(e) }.flatMap{e => e.overlappingParts(volume) }
  }

  def notifyAwake(e: Entity): Unit = { awakeEntities += e }
  def notifySleep(e: Entity): Unit = { awakeEntities -= e }
  def notifyDead(e: Entity): Unit = { awakeEntities -= e; allEntities -= e }
}
