package common.math

case object Even {
  def unapply[T](x: T)(implicit int: Integral[T]): Boolean = int.rem(x, int.fromInt(2)) == int.zero
}
case object Odd {
  def unapply[T](x: T)(implicit int: Integral[T]): Boolean = int.rem(x, int.fromInt(2)) == int.one
}
