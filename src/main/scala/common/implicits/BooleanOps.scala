package common.implicits

object BooleanOps {
  implicit class BooleanOps(x: Boolean) {
    def toInt: Int = if (x) 1 else 0
  }
}
