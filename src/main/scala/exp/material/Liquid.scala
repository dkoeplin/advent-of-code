package exp.material

abstract class Liquid(_name: String, _tension: Double) extends Material(_name) {
  override def liquid: Boolean = true
  val tension: Double = _tension
  lazy val tension2: Double = tension*tension
}
