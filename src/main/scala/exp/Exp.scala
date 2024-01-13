package exp
import common.immutable.Pos

import java.awt.GraphicsEnvironment
import java.util.TimerTask
import scala.swing.{Color, Frame, MainFrame}

object Exp extends scala.swing.SimpleSwingApplication {
  private def fps(n: Int): Int = 1000 / n

  val pixelsPerMeter: Double = 100
  val tickRate: Int = 10       // ms / tick
  val frameRate: Int = fps(30) // ms / redraw
  val gravity: Double = pixelsPerMeter * 9.8 * 1e-6 * tickRate*tickRate // 9.8 m/s^2 * (1/1000)^2 s^2/ms^2 * (ms / tick)^2
  val terminalVelocity: Double = pixelsPerMeter * 55 * 1e-3 * tickRate // 55 m/s * (1/1000) s/ms * (ms / tick)

  println(s"Gravity: $gravity pixels/tick^2")
  println(s"Terminal: $terminalVelocity pixels/tick")

  class Main extends MainFrame {
    title = s"Test"
    background = new Color(0, 0, 0)

    private val world = new World(this)
    contents = world

    def windowSize: Pos[Int] = {
      val wsize = peer.getBounds
      Pos(wsize.width, wsize.height)
    }

    private val gd = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
    if (gd.isFullScreenSupported) {
      // peer.setUndecorated(true)
      gd.setFullScreenWindow(peer)
      size = gd.getDefaultConfiguration.getBounds.getSize
    } else {
      peer.setUndecorated(true)
      peer.setExtendedState(peer.getExtendedState | java.awt.Frame.MAXIMIZED_BOTH)
    }

    peer.setVisible(true)

    private val timer = new java.util.Timer
    private val painter = new TimerTask { override def run(): Unit = repaint() }
    timer.scheduleAtFixedRate(world.clock, 0, tickRate)
    timer.scheduleAtFixedRate(painter, 0, frameRate)

    world.load()
    world.requestFocus()
  }

  override def top: Frame = new Main
}
