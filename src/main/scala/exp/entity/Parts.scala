package exp.entity

import common.immutable.{Border, Cube}
import exp.World

import scala.collection.immutable.ArraySeq

class Parts(parts: ArraySeq[Part]) {
  def this(parts: IterableOnce[Part]) = this(ArraySeq.from(parts))

  def iterator: Iterator[Part] = parts.iterator

  // TODO: O(N^2): Memoize or figure out how to make faster
  // TODO: Needs fixing
  private lazy val bordersAndDirs: ArraySeq[Border[Double]] = parts.flatMap{part =>
    val borders = part.volume.borders(width = 1, dist = 1e-9)
    parts.iterator.filterNot(_ == part).foldLeft(borders){(borders, part) =>
      borders.flatMap(_ diff part.volume).filter(_.thickness >= 1)
    }
  }

  object borders {
    def all: Iterator[Cube[Double]] = bordersAndDirs.iterator.map(_.volume)
    def up: Iterator[Cube[Double]] = bordersAndDirs.iterator.collect{case Border(dim, dir, b) if World.isVertical(dim) && dir(dim) < 0 => b }
    def notUp: Iterator[Border[Double]]
      = bordersAndDirs.iterator.collect{case border@Border(dim, dir, _) if !World.isVertical(dim) || dir(dim) > 0 => border }
  }

  def :+(part: Part): Parts = new Parts(parts :+ part)
  def map(func: Part => Part): Parts = new Parts(parts.map(func))
  def flatMap(func: Part => IterableOnce[Part]): Parts = new Parts(parts.flatMap(func))
}
object Parts {
  def empty: Parts = new Parts(ArraySeq.empty)
}
