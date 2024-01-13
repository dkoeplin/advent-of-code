package common.implicits

import scala.swing.Graphics2D

object Graphics2DOps {
  implicit class Graphics2DOps(g: Graphics2D) {
    def drawText(str: String, x: Int, y: Int): Unit = {
      g.drawChars(str.toCharArray, 0, str.length, x, y)
    }
  }
}
