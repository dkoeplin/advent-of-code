package exp.actor.entity

import common.immutable.{Box, Pos}
import common.mutable.BorderedRTree
import common.traits.RTreeHash
import exp.draw.Draw2D
import exp.material.Material

import scala.swing.Color

case class Part(box: Box[Long], material: Material, health: Int) {
  def this(box: Box[Long], material: Material) = this(box, material, material.durability)

  def overlaps(rhs: Box[Long]): Boolean = box.overlaps(rhs)
  def diff(rhs: Box[Long]): Iterator[Part] = (box diff rhs).map{v => Part(v, material, health) }

  def draw(g: Draw2D): Unit = {
    val alpha = (255 * Math.min(1, health.toDouble / material.durability)).toInt
    g.fillRect(box, new Color(material.color.getRed, material.color.getGreen, material.color.getBlue, alpha))
  }

  def highlight(g: Draw2D, brighter: Boolean): Unit
  = g.fillRect(box, if (brighter) material.color.brighter() else material.color.darker())
}

object Part {
  type Tree = BorderedRTree[Long, Part]
  val Tree: BorderedRTree.type = BorderedRTree

  implicit val PartHasher: RTreeHash[Long, Part] = new RTreeHash[Long, Part] {
    override def hash(value: Part): Long = value.hashCode().toLong

    override def box(value: Part): Box[Long] = value.box

    override def move(value: Part, delta: Pos[Long]): Part = value.copy(box = value.box + delta)
  }
}
