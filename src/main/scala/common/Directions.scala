package common

object Dir2D extends Enumeration {
  type Dir = Value
  val L, R, U, D = Value
}

object Dir3D extends Enumeration {
  type Dir = Value
  val L, R, U, D, F, B = Value
}
