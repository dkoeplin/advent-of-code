package exp

import common.immutable.{Pos, Volume}

import scala.swing.{Color, Graphics2D}

case class Ground(parent: World) extends Entity {
  lazy val vol: Volume[Int]
    = Volume(Pos(0,parent.windowSize.y - 40), Pos(parent.windowSize.x, parent.windowSize.y))

  override def tick(world: World): Unit = {}
  override val color: Color = new Color(32, 32, 32)
  override val volume: Volume[Double] = vol.toDoubles

  println(s"Window size: ${parent.windowSize}")
  println(s"Ground volume: $volume")

  override def draw(g: Graphics2D): Unit = {
    g.setColor(color)
    g.fill3DRect(vol.min.x, vol.min.y, vol.shape.x, vol.shape.y, true)
  }
}
