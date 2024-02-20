package exp

import common.immutable.Pos
import exp.draw.View2D

import java.awt.GraphicsEnvironment
import java.util.TimerTask
import scala.swing.{Color, Frame, MainFrame}

object Exp extends scala.swing.SimpleSwingApplication {
  private def fps(n: Long): Long = 1000 / n
  val frameRate: Long = fps(100) // ms / redraw

  val pxPerMeter: Long = 1000 // px / m
  val tickRate: Long = 30     // ms / tick

  class Main extends MainFrame {
    title = s"EXP"
    background = new Color(0, 0, 0)

    private val world = new World
    contents = world

    peer.setCursor {
      val emptyImage = new java.awt.image.BufferedImage(1, 1, java.awt.image.BufferedImage.TYPE_INT_ARGB)
      peer.getToolkit.createCustomCursor(emptyImage, new scala.swing.Point(), null)
    }

    def windowSize: Pos[Int] = {
      val wsize = peer.getBounds
      Pos(wsize.width, wsize.height)
    }

    val device: java.awt.GraphicsDevice = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
    if (device.isFullScreenSupported) {
      // peer.setUndecorated(true)
      device.setFullScreenWindow(peer)
      size = device.getDefaultConfiguration.getBounds.getSize
    } else {
      peer.setUndecorated(true)
      peer.setExtendedState(peer.getExtendedState | java.awt.Frame.MAXIMIZED_BOTH)
    }
    peer.setVisible(true)

    private val timer = new java.util.Timer
    timer.scheduleAtFixedRate(new TimerTask {
      override def run(): Unit = {
        world.tick()
        repaint()
      }
    }, 0, tickRate)
    // timer.scheduleAtFixedRate(new TimerTask{ override def run(): Unit = repaint() }, 0, frameRate)

    val view = new View2D(this)
    world.requestFocus()
    world.setView(view)
    world.reset(windowSize.toLongs)
  }

  override def top: Frame = new Main
}
