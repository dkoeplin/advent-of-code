package exp.window

import common.implicits.Graphics2DOps._
import exp.World

import scala.swing.{Color, Component, Graphics2D}

class Debug(world: World) extends Component {
  override def paint(g: Graphics2D): Unit = {
    g.setColor(new Color(0, 0, 0))
    g.drawText(s"Alive: ${world.entities.awake.size}", 0, 20)
    world.entities.all.foreach{e =>
      val b = e.bbox.roundInt
      g.setColor(new Color(255, 0, 0))
      g.drawRect(b.min.x, b.min.y, b.shape.x, b.shape.y)
    }
  }
}
