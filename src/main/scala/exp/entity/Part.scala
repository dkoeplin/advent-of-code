package exp.entity

import common.immutable.{Cube, Pos}
import exp.draw.Draw2D
import exp.material.Material

case class Part(volume: Cube[Long], material: Material) {
  def overlaps(rhs: Cube[Long]): Boolean = volume.overlaps(rhs)
  def diff(rhs: Cube[Long]): Iterator[Part] = (volume diff rhs).map{v => Part(v, material) }

  def +(pos: Pos[Long]): Part = Part(volume + pos, material)

  def *(rhs: Long): Part = Part(volume * rhs, material)

  def draw(g: Draw2D): Unit = g.fillRect(volume, material.color)
}
