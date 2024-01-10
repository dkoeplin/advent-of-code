package common.viz

import scala.swing.{Frame, Graphics2D}

trait Visualizer {
  def apply(frame: Frame, g: Graphics2D): Unit
}
