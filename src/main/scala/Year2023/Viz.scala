package Year2023

import common.immutable.Pos.Idx

import java.awt.Color
import scala.swing._
import scala.util.Random

object Viz extends SimpleSwingApplication {
  val bg = new Color(1, 20, 105)
  val red = new Color(255, 0, 0)
  val green = new Color(0, 255, 0)

  implicit class GraphicsOps(g: Graphics2D) {
    def up(pos: Idx, h: Int, w: Int): Unit = {
      g setColor green
      g.fillPolygon(Array(pos.w, pos.w + w/2, pos.w + w), Array(pos.h, pos.h - h, pos.h), 3)
    }
    def down(pos: Idx, h: Int, w: Int): Unit = {
      g setColor red
      g.fillPolygon(Array(pos.w, pos.w + w/2, pos.w + w), Array(pos.h - h, pos.h, pos.h - h), 3)
    }
    def rectangle(pos: Idx, h: Int, w: Int): Unit = {
      g setColor bg
      g fillRect(pos.w, pos.h, w, h)
    }
  }

  def paintGraphics(g: Graphics2D): Unit = {
    // g.rectangle(Pos(0, 0), g., 1000)
    (0 until 5).iterator.foreach{i =>
      (0 until 3).foreach{j =>
        val pos = Idx(600 + i * 100, 50 + j * 200)
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
