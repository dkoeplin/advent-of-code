package common.mutable

class SparseMatrix[T](default: T) extends SparseTensor[T](2, default) {

  override def toString: String = {
    val volume = min to max
    volume.iterator.map(apply).grouped(volume.W).map{_.map{
      case x: Int => Integer.toHexString(x)
      case x => x.toString
    }.mkString("")}.mkString("\n")
  }
}
