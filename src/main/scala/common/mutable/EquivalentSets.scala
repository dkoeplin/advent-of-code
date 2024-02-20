package common.mutable

import scala.collection.mutable

class EquivalentSets[A] {
  def add(a: A, b: A): Unit = add(List(a, b))

  def add(items: Iterable[A]): Unit = {
    val indices: Set[Long] = items.flatMap(lookup.get).toSet
    val next = indices.flatMap(groups.apply) ++ items
    groups.subtractAll(indices)
    groups(count) = next
    next.foreach { item => lookup(item) = count }
    count += 1
  }

  def apply(a: A): A = groups.get(lookup(a)).map(_.head).getOrElse {
    throw new Exception("No element found")
  }
  def sets: Iterator[Set[A]] = groups.valuesIterator

  private var count: Long = 0
  private val groups: mutable.HashMap[Long, Set[A]] = mutable.HashMap.empty
  private val lookup: mutable.HashMap[A, Long] = mutable.HashMap.empty
}
object EquivalentSets {
  def empty[A]: EquivalentSets[A] = new EquivalentSets[A]
}
