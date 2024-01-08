package common.implicits

import common.math.{Even, Odd}

object MathOps {
  implicit class EvenOdd[T:Integral](x: T) {
    def isEven: Boolean = Even.unapply(x)
    def isOdd: Boolean = Odd.unapply(x)
  }
}
