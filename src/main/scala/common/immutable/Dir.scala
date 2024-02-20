package common.immutable

sealed abstract class Dir {
  def unary_- : Dir
}

object Dir {
  case object Pos extends Dir { override def unary_- : Dir = Neg }
  case object Neg extends Dir { override def unary_- : Dir = Pos }

  val list: List[Dir] = List(Neg, Pos)
  def iterator: Iterator[Dir] = list.iterator
}
