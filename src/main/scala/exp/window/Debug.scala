package exp.window

import common.implicits.Graphics2DOps._
import exp.World

import scala.swing.{Color, Component, Graphics2D}

class Debug(world: World) extends Component {
  override def paint(g: Graphics2D): Unit = {
    g.setColor(new Color(0, 0, 0))
    g.drawText(s"Alive: ${world.entities.all.size}", 10, 20)
    g.drawText(s"Awake: ${world.entities.awake.size}", 10, 40)
    g.drawText(s"Tools: ${world.Tool.current}", 10, 60)
    g.drawText(s"Key: ${world.prevKey.map(_.toString).getOrElse("N/A")}", 10, 80)
    world.entities.awake.zipWithIndex.foreach{case (e, i) =>
      val b = e.bbox.roundInt
      g.setColor(new Color(255, 0, 0))
      g.drawRect(b.min.x, b.min.y, b.shape.x, b.shape.y)
      g.drawText(e.toString, 300, 20 + 20*i)
    }
  }
}
