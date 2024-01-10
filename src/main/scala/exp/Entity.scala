package exp

import common.immutable.Volume

import scala.swing.{Color, Graphics2D}

trait Entity {
  def tick(world: World): Unit
  def volume: Volume[Double]
  def color: Color
  var id: Int = 0

  override def hashCode(): Int = id.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case e: Entity => id == e.id
    case _ => false
  }
  def ==(rhs: Entity): Boolean = id == rhs.id
  def !=(rhs: Entity): Boolean = id != rhs.id

  def draw(g: Graphics2D): Unit
}
