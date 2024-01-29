package exp.actor.entity

import common.immutable.{Border, Box}
import common.mutable.RTree

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

class PartsBuilder(rank: Int) {
  val parts: RTree[Long,Part] = RTree.empty[Long,Part](rank)
  var borders: ArraySeq[Border[Long]] = ArraySeq.empty[Border[Long]]

  private def nearby(v: Box[Long]): Iterator[Part] = parts(v).iterator

  def finish: Parts = new Parts(parts, borders)

  /// Perform the addition of this part without actually adding it yet
  def prepareToAdd(part: Part): Boolean = {
    val (overlap, nonoverlap) = borders.iterator.partition{b => b.volume.overlaps(part.box) }
    if (overlap.nonEmpty) {
      borders = ArraySeq.from(nonoverlap ++ overlap.flatMap{b => b diff part.box })
      true
    } else false
  }
  def finishAdding(part: Part): Unit = {
    parts += part
    borders = borders ++ part.box.borders().flatMap{ b => b diff nearby(b.volume).map(_.box) }
  }

  def +=(part: Part): Unit = {
    prepareToAdd(part)
    finishAdding(part)
  }
}
object PartsBuilder {
  def empty(rank: Int): PartsBuilder = new PartsBuilder(rank)
}

class PartsGroupBuilder {
  var count: Int = 0
  val groups: mutable.HashMap[Int, PartsBuilder] = mutable.HashMap.empty
  val lookup: mutable.HashMap[Part, Int] = mutable.HashMap.empty

  def +=(part: Part): Unit = {
    val sets = groups.iterator.filter{case (_, group) => group.prepareToAdd(part) }.map(_._1).toArray

    val idx = if (sets.length == 1) sets.head else {
      count += 1
      val result = PartsBuilder.empty(part.box.rank)
      val builders = sets.map(groups.apply)
      result.parts ++= builders.iterator.flatMap{_.parts.iterator}
      result.borders = ArraySeq.from(builders.iterator.flatMap{_.borders.iterator})
      groups(count) = result
      groups.subtractAll(sets)
      result.parts.foreach{part => lookup(part) = count }
      count
    }
    val result = groups(idx)
    result.finishAdding(part)
    lookup(part) = idx
  }
  def ++=(parts: IterableOnce[Part]): Unit = parts.iterator.foreach{part => this += part }

  def finish: Iterator[Parts] = groups.valuesIterator.map(_.finish)

  def size: Int = groups.size
  def isEmpty: Boolean = lookup.isEmpty
}
object PartsGroupBuilder {
  def empty: PartsGroupBuilder = new PartsGroupBuilder
}