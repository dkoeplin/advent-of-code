package exp.draw

import common.immutable.{Cube, Pos}
import exp.World

class View2D(world: World) {
  private val _robot: java.awt.Robot = new java.awt.Robot(world.parent.device)
  private var _offset: Pos[Int] = world.parent.peer.getLocationOnScreen
  private var _center: Pos[Int] = world.parent.windowSize / 2
  private var _focus: Pos[Double] = world.parent.windowSize.toDoubles / 2
  private var _view: Cube[Double] = Cube(Pos(0,0), world.parent.windowSize.toDoubles)

  var prev: Pos[Int] = Pos(0, 0)
  var offset: Pos[Int] = _offset
  def center: Pos[Int] = _center
  def focus: Pos[Double] = _focus
  def range: Cube[Double] = _view

  // Scale from world coordinates to view coordinates (allows for out of range)
  def scale(cube: Cube[Double]): Cube[Int] = (cube - range.min).roundInt
  def scale(pos: Pos[Double]): Pos[Int] = (pos - range.min).roundInt

  /*def resize(ofs: Pos[Int], size: Pos[Int]): this.type = {
    _offset = ofs
    _center = _offset + (size / 2)
    _focus = size.toDoubles / 2
    _view = Cube(Pos(0, 0), size.toDoubles)
    this
  }*/

  var delay: Int = 0
  def move(pt: Pos[Int]): this.type = {
    if (delay < 1000) { delay += 1; prev = pt } else {
      val delta = (pt - center).toDoubles
      _focus = _focus + delta
      _view = _view + delta
    }
    _robot.mouseMove(center.x + offset.x, center.y + offset.y)
    this
  }

  def recenter(): this.type = {
    _focus = center.toDoubles / 2
    _view = _view - _view.min
    this
  }
}