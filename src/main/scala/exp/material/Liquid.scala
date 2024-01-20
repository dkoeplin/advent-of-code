package exp.material

abstract class Liquid(_name: String, _tension: Long) extends Material(_name) {
  override def liquid: Boolean = true
  val tension: Long = _tension
  lazy val tension2: Long = tension*tension
}
