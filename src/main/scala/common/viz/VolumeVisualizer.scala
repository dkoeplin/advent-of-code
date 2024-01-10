package common.viz

import common.immutable.{Pos, Volume}

import scala.swing.{Color, Frame, Graphics2D}

case class VolumeVisualizer[A](volumes: IterableOnce[Volume[A]]) extends Visualizer {
  private lazy val RGB_MAX: Int = 1 << 23

  def apply(frame: Frame, g: Graphics2D): Unit = {
    val (a, b) = volumes.iterator.duplicate
    val bounds = b.map(_.keepDims(1, 2)).reduce(_ union _)
    val wsize = frame.peer.getBounds
    val wDims = Pos(wsize.getHeight, wsize.getWidth)
    val scaling: Pos[Double] = wDims / bounds.shape.toDoubles

    val scaled = a.map{v =>
      val res = ((v.keepDims(1, 2) + bounds.min).toDoubles * scaling).toInts
      println(s"$v => $res")
      res
    }

    scaled.zipWithIndex.foreach{case (v, i) =>
      val color = new Color(util.Random.nextInt(255), util.Random.nextInt(255), util.Random.nextInt(255))
      g.setColor(color)
      g.fillRect(v.min.h, v.min.w, v.shape.h, v.shape.w)
    }
  }
}
