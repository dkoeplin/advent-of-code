package exp.material
import scala.swing.Color

case object Bedrock extends Material("Bedrock") {
  override val color: Color = new Color(32, 32, 32)
  override val falls: Boolean = false
  override val immortal: Boolean = true
  override val durability: Int = 1000
}
