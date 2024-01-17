package common.mutable

import scala.collection.mutable

class EquivalentSets[A] {
  case class Entry(set: Set[A], idx: Int)

  def add(a: A, b: A): Unit = add(Iterator(a, b))
  def add(items: IterableOnce[A]): Unit = {
    val sets: Set[Int] = items.iterator.flatMap(lookup.get).toSet
    val next: Set[A] = sets.flatMap(groups.apply) ++ items
    groups(count) = next
    items.iterator.foreach{item => lookup(item) = count }
    groups.subtractAll(sets)
    count += 1
  }

  def sets: Iterator[Set[A]] = groups.valuesIterator

  private var count: Int = 0
  private val groups: mutable.HashMap[Int, Set[A]] = mutable.HashMap.empty
  private val lookup: mutable.HashMap[A, Int] = mutable.HashMap.empty
}
object EquivalentSets {
  def empty[A]: EquivalentSets[A] = new EquivalentSets[A]
}
