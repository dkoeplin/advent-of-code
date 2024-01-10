package common

import common.viz.Visualizer

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.io.BufferedSource
import scala.swing.{Frame, Graphics2D, MainFrame, Panel}

class AoC(day: Int, year: Int) extends App {
  private val dayStr = day.toString
  private val pad = "0" * (2 - dayStr.length)
  private def exampleName(n: Int): String = s"example/$year/$pad$dayStr${if (n >= 0) s"-$n" else ""}"
  def example(n: Int = -1): BufferedSource = io.Source.fromFile(exampleName(n))

  lazy val data: BufferedSource = {
    val filename = s"data/$year/$pad$day"
    val filename2 = s"data/$year/$day"
    if (java.nio.file.Files.exists(java.nio.file.Paths.get(filename))) {
      io.Source.fromFile(filename)
    } else if (java.nio.file.Files.exists(java.nio.file.Paths.get(filename2))) {
      io.Source.fromFile(filename2)
    } else {
      assert(assertion=false, s"Cannot find data $filename or $filename2")
      null
    }
  }

  def write(filename: String, str: String): Unit = {
    Files.write(Paths.get(filename), str.getBytes(StandardCharsets.UTF_8))
  }

  val visualizers: ArrayBuffer[Visualizer] = ArrayBuffer.empty

  case class Visualizations(frame: Frame, vs: ArrayBuffer[Visualizer]) extends Panel {
    override def paint(g: Graphics2D): Unit = vs.foreach(_.apply(frame, g))
  }

  lazy val viz: Frame = new MainFrame {
    title = s"AoC Day $day Year $year"
    //  background =
    resizable = true
    contents = Visualizations(this, visualizers)
    maximize()
  }
  def show(): Unit = { viz.visible = true }
}
