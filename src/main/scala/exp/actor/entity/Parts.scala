package exp.actor.entity

import common.immutable.{Border, Box}
import common.mutable.RTree
import exp.World
import exp.material.Material

import scala.collection.immutable.ArraySeq

class Parts(parts: RTree[Long,Part], val _borders: ArraySeq[Border[Long]]) {
  def this(vol: Box[Long], mat: Material) = this(RTree.single[Long,Part](new Part(vol, mat)), ArraySeq.from(vol.borders()))
  def size: Int = parts.size
  def iterator: Iterator[Part] = parts.iterator

  object borders {
    def notUp: Iterator[Border[Long]]
      = _borders.iterator.collect{case border@Border(dim, dir, _) if !World.isVertical(dim) || dir(dim) > 0 => border }
  }

  object bounds {
    def all: Iterator[Box[Long]] = _borders.iterator.map(_.volume)
    def up: Iterator[Box[Long]] = _borders.iterator.collect{case Border(dim, dir, b) if World.isVertical(dim) && dir(dim) < 0 => b }
  }

  def map(func: Part => Part): Parts = {
    val builder = new PartsBuilder(parts.rank)
    foreach{part => builder += func(part) }
    builder.finish
  }
  // def flatMap(func: Part => IterableOnce[Part]): Parts = new Parts(parts.flatMap(func))
  def foreach(func: Part => Unit): Unit = parts.foreach(func)
}
