package exp.entity

import common.immutable.{Border, Cube}
import exp.World

import scala.collection.immutable.ArraySeq

class Parts(parts: ArraySeq[Part]) {
  def this(parts: IterableOnce[Part]) = this(ArraySeq.from(parts))

  def iterator: Iterator[Part] = parts.iterator

  // TODO: O(N^2): Memoize or figure out how to make faster
  // TODO: Needs fixing
  private lazy val bordersAndDirs: ArraySeq[Border[Long]] = parts.flatMap{part =>
    val borders = part.volume.borders()
    parts.iterator.filterNot(_ == part).foldLeft(borders){(borders, part) => borders.flatMap(_ diff part.volume) }
  }

  object borders {
    def all: Iterator[Cube[Long]] = bordersAndDirs.iterator.map(_.volume)
    def up: Iterator[Cube[Long]] = bordersAndDirs.iterator.collect{case Border(dim, dir, b) if World.isVertical(dim) && dir(dim) < 0 => b }
    def notUp: Iterator[Border[Long]]
      = bordersAndDirs.iterator.collect{case border@Border(dim, dir, _) if !World.isVertical(dim) || dir(dim) > 0 => border }
  }

  def :+(part: Part): Parts = new Parts(parts :+ part)
  def map(func: Part => Part): Parts = new Parts(parts.map(func))
  def flatMap(func: Part => IterableOnce[Part]): Parts = new Parts(parts.flatMap(func))
  def foreach(func: Part => Unit): Unit = parts.foreach(func)
}
object Parts {
  def empty: Parts = new Parts(ArraySeq.empty)
}
