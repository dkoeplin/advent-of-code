package exp.actor.entity

import common.immutable.{Box, Pos}
import common.traits.HasBox
import exp.draw.Draw2D
import exp.material.Material

import scala.swing.Color

case class Part(box: Box[Long], material: Material, health: Int) {
  def this(volume: Box[Long], material: Material) = this(volume, material, material.durability)

  def overlaps(rhs: Box[Long]): Boolean = box.overlaps(rhs)
  def diff(rhs: Box[Long]): Iterator[Part] = (box diff rhs).map{ v => Part(v, material, health) }

  def +(pos: Pos[Long]): Part = Part(box + pos, material, health)

  def *(rhs: Long): Part = Part(box * rhs, material, health)

  def draw(g: Draw2D): Unit = {
    val alpha = (255 * Math.min(1, health.toDouble / material.durability)).toInt
    g.fillRect(box, new Color(material.color.getRed, material.color.getGreen, material.color.getBlue, alpha))
  }
  def highlight(g: Draw2D, brighter: Boolean): Unit
    = g.fillRect(box, if (brighter) material.color.brighter() else material.color.darker())
}
object Part {
  implicit val PartHasBox: HasBox[Long,Part] = _.box
}
