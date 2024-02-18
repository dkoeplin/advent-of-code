package exp.actor.entity

import common.immutable.{Border, Box, Pos}
import common.mutable.RTree
import common.traits.RTreeHash
import exp.World
import exp.draw.Draw2D
import exp.material.Material

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.swing.Color

class Parts private (rank: Int) {
  protected val _tree: RTree[Long,Parts.PartView] = RTree.empty[Long,Parts.PartView](rank)
  protected var _borders: ArraySeq[Border[Long]] = ArraySeq.empty[Border[Long]]

  def loc: Pos[Long] = _tree.loc
  def shape: Pos[Long] = _tree.shape
  def bbox: Box[Long] = _tree.bbox
  def tree: RTree[Long, Parts.PartView] = _tree

  def iterator: Iterator[Part] = _tree.iterator.map(_.at(loc))
  def foreach(func: Part => Unit): Unit = iterator.foreach(func)

  def moveto(pos: Pos[Long]): Unit = {
    val delta = pos - loc
    _tree.moveto(pos)
    _borders = _borders.map(_ + delta)
  }

  def draw(g: Draw2D): Unit = g.at(loc){ _tree.iterator.foreach(_.draw(g)) }
  def highlight(g: Draw2D, brighter: Boolean): Unit = g.at(loc){ _tree.iterator.foreach(_.highlight(g, brighter)) }

  object borders {
    def all: Iterator[Border[Long]] = _borders.iterator
    def notUp: Iterator[Border[Long]]
      = _borders.iterator.collect{case border@Border(dim, dir, _) if !World.isVertical(dim) || dir(dim) > 0 => border }
  }

  object bounds {
    def all: Iterator[Box[Long]] = _borders.iterator.map(_.volume)
    def up: Iterator[Box[Long]] = _borders.iterator.collect{case Border(dim, dir, b) if World.isVertical(dim) && dir(dim) < 0 => b }
  }
}

object Parts {
  protected class PartView(private val box: Box[Long], val material: Material, val health: Int) {
    def draw(g: Draw2D): Unit = {
      val alpha = (255 * Math.min(1, health.toDouble / material.durability)).toInt
      g.fillRect(box, new Color(material.color.getRed, material.color.getGreen, material.color.getBlue, alpha))
    }

    def highlight(g: Draw2D, brighter: Boolean): Unit
      = g.fillRect(box, if (brighter) material.color.brighter() else material.color.darker())

    def at(offset: Pos[Long]): Part = Part(box + offset, material, health)

    override def toString: String = box.toString
  }
  protected object PartView {
    implicit val PartHasBox: RTreeHash[Long,PartView] = new RTreeHash[Long,PartView] {
      override def hash(value: PartView): Long = value.hashCode().toLong
      override def box(value: PartView): Box[Long] = value.box
    }
  }

  private implicit class PartOps(part: Part) {
    def view: Parts.PartView = new PartView(part.box, part.material, part.health)
  }

  private implicit class Builder(parts: Parts) {
    def nearby(v: Box[Long]): Iterator[Part] = parts._tree(v).iterator.map(_.at(parts.loc))

    /// Perform the addition of this part without actually adding it yet
    def prepareToAdd(part: Part): Boolean = {
      val (overlap, nonoverlap) = parts._borders.iterator.partition{b => b.volume.overlaps(part.box) }
      if (overlap.nonEmpty) {
        parts._borders = ArraySeq.from(nonoverlap ++ overlap.flatMap { b => b diff part.box })
        true
      } else false
    }

    def finishAdding(part: Part): Unit = {
      parts._tree += part.view
      parts._borders = parts._borders ++ part.box.borders().flatMap { b => b diff nearby(b.volume).map(_.box) }
    }

    def +=(part: Part): Unit = {
      prepareToAdd(part)
      finishAdding(part)
    }
  }

  class Grouping protected {
    var count: Int = 0
    val groups: mutable.HashMap[Int, Parts] = mutable.HashMap.empty
    val lookup: mutable.HashMap[Part, Int] = mutable.HashMap.empty

    def +=(part: Part): Unit = {
      val sets = groups.iterator.filter{case (_, group) => group.prepareToAdd(part) }.map(_._1).toArray

      val idx = if (sets.length == 1) sets.head else {
        count += 1
        val result = new Parts(part.box.rank)

        val builders = sets.map(groups.apply)
        result._tree ++= builders.iterator.flatMap{_.iterator.map(_.view)}
        result._borders = ArraySeq.from(builders.iterator.flatMap{_.borders.all})
        groups(count) = result
        groups.subtractAll(sets)
        result.iterator.foreach{part => lookup(part) = count }
        count
      }
      val result = groups(idx)
      result.finishAdding(part)
      lookup(part) = idx
    }
    def ++=(parts: IterableOnce[Part]): Unit = parts.iterator.foreach{part => this += part }

    def finish: Iterable[Parts] = groups.values

    def size: Int = groups.size
    def isEmpty: Boolean = lookup.isEmpty
  }
  object Grouping {
    def empty: Grouping = new Grouping
  }

  def single(box: Box[Long], material: Material): Parts = {
    val parts = new Parts(box.rank)
    parts += Part(box, material, material.durability)
    parts
  }
}
