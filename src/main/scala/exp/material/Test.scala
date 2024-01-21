package exp.material

import scala.swing.Color

case class Test(color: Color) extends Material("test") {
  override def durability: Int = 2
}
object Test {
  def random: Test = Test(new Color(util.Random.nextInt(255), util.Random.nextInt(255), util.Random.nextInt(255)))
}
