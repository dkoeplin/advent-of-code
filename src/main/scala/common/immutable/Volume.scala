package common.immutable

trait Volume[T] {
  def size: T
  def rank: Int
}
