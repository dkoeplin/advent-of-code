package exp.screen

import exp.draw.Draw2D

abstract class Screen {
  def draw(g: Draw2D): Unit
}
