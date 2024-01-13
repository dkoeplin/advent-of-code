package exp

import scala.swing.Color

abstract class Material(_name: String) {
  def name: String = _name
  def color: Color
  def falls: Boolean = true
  def liquid: Boolean = false
  def immortal: Boolean = false
}
