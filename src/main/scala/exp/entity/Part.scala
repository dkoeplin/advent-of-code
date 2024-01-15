package exp.entity

import common.immutable.{Cube, Pos}
import exp.Material

import scala.swing.Graphics2D

case class Part(volume: Cube[Double], material: Material) {
  def +(pos: Pos[Double]): Part = Part(volume + pos, material)

  def *(rhs: Double): Part = Part(volume * rhs, material)

  def draw(g: Graphics2D): Unit = {
    val v = volume.roundInt
    g.setColor(material.color)
    g.fillRect(v.min.x, v.min.y, v.shape.x, v.shape.y)
  }
}
