package exp.entity

import common.immutable.{Cube, Pos}
import exp.draw.Draw2D
import exp.material.Material

case class Part(volume: Cube[Double], material: Material) {
  def +(pos: Pos[Double]): Part = Part(volume + pos, material)

  def *(rhs: Double): Part = Part(volume * rhs, material)

  def draw(g: Draw2D): Unit = g.fillRect(volume, material.color)
}
