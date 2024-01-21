package exp.screen

import common.immutable.Pos
import exp.World
import exp.actor.entity.Entity
import exp.draw.Draw2D

class Debug(world: World) extends Screen {
  override def draw(g: Draw2D): Unit = {
    g.window.drawText(s"Alive: ${world.actors.entities.size}", Pos(10, 20))
    g.window.drawText(s"Awake: ${world.actors.awake.size}", Pos(10, 40))
    g.window.drawText(s"Tools: ${world.Tool.current}", Pos(10, 60))
    g.window.drawText(s"Key: ${world.prevKey.map(_.toString).getOrElse("N/A")}", Pos(10, 80))
    g.window.drawText(s"Focus: ${world.view.map(_.focus.toString).getOrElse("N/A")}", Pos(10, 100))
    g.window.drawText(s"Range: ${world.view.map(_.range.toString).getOrElse("N/A")}", Pos(10, 120))
    world.actors.awake.foreach{
      case e: Entity => e.borders.foreach{border => g.fillRect(border, Draw2D.kRed) }
      case _ =>
    }
  }
}
