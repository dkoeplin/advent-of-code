package common.implicits

import common.immutable.Pos

import scala.language.implicitConversions

object TupleOps {
  implicit def PosFromTuple2[A:Numeric](x: (A,A)): Pos[A] = Pos(x._1, x._2)
  implicit def PosFromTuple3[A:Numeric](x: (A,A,A)): Pos[A] = Pos(x._1, x._2, x._3)
}
