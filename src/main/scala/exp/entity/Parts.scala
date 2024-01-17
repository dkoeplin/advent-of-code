package exp.entity

import common.immutable.{Cube, Pos}

import scala.collection.immutable.ArraySeq

import exp.World

class Parts(parts: ArraySeq[Part]) {
  def this(parts: IterableOnce[Part]) = this(ArraySeq.from(parts))

  def iterator: Iterator[Part] = parts.iterator

  // TODO: O(N^2): Memoize or figure out how to make faster
  private lazy val bordersAndDirs = ArraySeq.from(iterator.flatMap{part =>
    part.volume.dirsAndBorders(Entity.kUpdateRange).filterNot{case (_, b) =>
      iterator.exists{p => p != part && p.volume.overlaps(b) }
    }
  })

  lazy val around: ArraySeq[Cube[Double]] = bordersAndDirs.map(_._2)
  lazy val up: ArraySeq[Cube[Double]] = bordersAndDirs.collect{case (dir, b) if World.isVertical(dir) && dir.magnitude < 0 => b}

  def :+(part: Part): Parts = new Parts(parts :+ part)
  def map(func: Part => Part): Parts = new Parts(parts.map(func))
  def flatMap(func: Part => IterableOnce[Part]): Parts = new Parts(parts.flatMap(func))
}
object Parts {
  def empty: Parts = new Parts(ArraySeq.empty)
}
