package exp.draw

import common.immutable.{Cube, Pos}

import scala.swing.Color

class Draw2D(g: scala.swing.Graphics2D, val view: View2D) {
  def fillRect(x: Cube[Double], color: Color): Unit = window.fillRect(view.scale(x), color)
  def lineRect(x: Cube[Double], color: Color): Unit = window.lineRect(view.scale(x), color)

  def drawText(str: String, pos: Pos[Double], color: Color = Draw2D.kBlack): Unit = window.drawText(str, view.scale(pos), color)

  // Drawing directly in window coordinates
  object window {
    def fillRect(cube: Cube[Int], color: Color): Unit = {
      g.setColor(color)
      g.fillRect(cube.min.x, cube.min.y, cube.shape.x, cube.shape.y)
    }
    def lineRect(cube: Cube[Int], color: Color): Unit = {
      g.setColor(color)
      g.drawRect(cube.min.x, cube.min.y, cube.shape.x, cube.shape.y)
    }
    def drawText(str: String, pos: Pos[Int], color: Color = Draw2D.kBlack): Unit = {
      g.setColor(color)
      g.drawChars(str.toCharArray, 0, str.length, pos.x, pos.y)
    }
  }
}
object Draw2D {
  val kBlack = new Color(0, 0, 0)
  val kRed = new Color(255, 0, 0)
}