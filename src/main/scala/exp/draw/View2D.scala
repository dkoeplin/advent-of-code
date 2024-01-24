package exp.draw

import common.immutable.{Box, Pos}
import exp.World

class View2D(world: World) {
  private val _robot: java.awt.Robot = new java.awt.Robot(world.parent.device)
  private var _focus: Pos[Long] = world.parent.windowSize.toLongs / 2L

  private def offset: Pos[Int] = world.parent.peer.getLocationOnScreen
  def center: Pos[Int] = world.parent.windowSize / 2

  def focus: Pos[Long] = _focus
  def range: Box[Long] = {
    val half = center.toLongs
    Box(focus - half, focus + half)
  }

  // Scale from world coordinates to view coordinates (allows for out of range)
  def scale(cube: Box[Long]): Box[Int] = (cube - range.min).toInts
  def scale(pos: Pos[Long]): Pos[Int] = (pos - range.min).toInts

  /*def resize(ofs: Pos[Int], size: Pos[Int]): this.type = {
    _offset = ofs
    _center = _offset + (size / 2)
    _focus = size.toDoubles / 2
    _view = Cube(Pos(0, 0), size.toDoubles)
    this
  }*/

  var centered: Int = 10
  def move(pt: Pos[Int]): this.type = {
    if (centered < 100) { centered += 1 } else {
      val delta = (pt - center).toLongs
      _focus = _focus + delta
    }
    _robot.mouseMove(center.x + offset.x, center.y + offset.y)
    this
  }

  def recenter(): Unit = { _focus = center.toLongs }
}