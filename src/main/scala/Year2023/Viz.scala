package Year2023

import common.Pos

import scala.swing._
import java.awt.Color
import scala.util.Random

object Viz extends SimpleSwingApplication {
  val bg = new Color(1, 20, 105)
  val red = new Color(255, 0, 0)
  val green = new Color(0, 255, 0)

  implicit class GraphicsOps(g: Graphics2D) {
    def up(pos: Pos, h: Int, w: Int): Unit = {
      g setColor green
      g.fillPolygon(Array(pos.col, pos.col + w/2, pos.col + w), Array(pos.row, pos.row - h, pos.row), 3)
    }
    def down(pos: Pos, h: Int, w: Int): Unit = {
      g setColor red
      g.fillPolygon(Array(pos.col, pos.col + w/2, pos.col + w), Array(pos.row - h, pos.row, pos.row - h), 3)
    }
    def rectangle(pos: Pos, h: Int, w: Int): Unit = {
      g setColor bg
      g fillRect(pos.col, pos.row, w, h)
    }
  }

  def paintGraphics(g: Graphics2D): Unit = {
    // g.rectangle(Pos(0, 0), g., 1000)
    (0 until 5).iterator.foreach{i =>
      (0 until 3).foreach{j =>
        val pos = Pos(600 + i * 100, 50 + j * 200)
        if (Random.nextBoolean()) g.up(pos, 20, 20) else g.down(pos, 20, 20)
      }
    }
  }

  class MyFrame extends MainFrame {
    title = "NYSE"
    background = bg
    resizable = true
    maximize()
    contents = new Panel {
      override def paint(g: Graphics2D): Unit = {
        paintGraphics(g)
      }
    }
  }

  override def top = new MyFrame
}
