package exp

import common.immutable.Pos

import java.awt.GraphicsEnvironment
import java.util.TimerTask
import scala.swing.{Color, Frame, MainFrame}

object Exp extends scala.swing.SimpleSwingApplication {
  // private def fps(n: Long): Long = 1000 / n
  // val frameRate: Long = fps(30) // ms / redraw

  val pxPerMeter: Long = 1000 // px / m
  val tickRate: Long = 30     // ms / tick

  class Main extends MainFrame {
    title = s"EXP"
    background = new Color(0, 0, 0)

    protected val emptyCursor: java.awt.Cursor
    = peer.getToolkit.createCustomCursor(new java.awt.image.BufferedImage(1, 1, java.awt.image.BufferedImage.TYPE_INT_ARGB),
      new scala.swing.Point(), null)
    peer.setCursor(emptyCursor)

    private val world = new World(this)
    contents = world

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
    private val tick = new TimerTask { override def run(): Unit = {
      world.tick()
      repaint()
    }}
    // timer.scheduleAtFixedRate(world.clock, 0, tickRate)
    timer.scheduleAtFixedRate(tick, 0, tickRate)

    world.load()
    world.requestFocus()
  }

  override def top: Frame = new Main
}
