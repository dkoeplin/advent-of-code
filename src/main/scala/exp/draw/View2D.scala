package exp.draw

import common.immutable.{Cube, Pos}

import java.awt.GraphicsDevice

class View2D(screen: GraphicsDevice, window: Pos[Int]) {
  private val _robot: java.awt.Robot = new java.awt.Robot(screen)
  private var _center: Pos[Int] = window / 2
  private var _focus: Pos[Double] = window.toDoubles / 2
  private var _view: Cube[Double] = Cube(Pos(0,0), window.toDoubles)

  var prev: Option[Pos[Int]] = None
  def center: Pos[Int] = _center
  def focus: Pos[Double] = _focus
  def range: Cube[Double] = _view

  // Scale from world coordinates to view coordinates (allows for out of range)
  def scale(cube: Cube[Double]): Cube[Int] = (cube - range.min).roundInt
  def scale(pos: Pos[Double]): Pos[Int] = (pos - range.min).roundInt

  def resize(size: Pos[Int]): this.type = {
    _center = size / 2
    _focus = size.toDoubles / 2
    _view = Cube(Pos(0, 0), size.toDoubles)
    this
  }

  def move(pt: Pos[Int]): this.type = {
    //println(s"Move to pt $pt vs center $center ==> $delta")
    prev.foreach{p =>
      val delta = (pt - p).toDoubles
      _focus = _focus + delta
      _view = _view + delta
    }
    prev = Some(pt)
    _robot.mouseMove(center.x, center.y)
    this
  }
  def recenter(): this.type = {
    _focus = center.toDoubles / 2
    _view = _view - _view.min
    this
  }
}