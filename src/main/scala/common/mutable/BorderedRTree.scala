package common.mutable

import common.immutable.{Border, Box, Pos, View}
import common.traits.RTreeHash

/**
 * Implements an RTree with quick border lookup using a pair of RTrees.
 */
class BorderedRTree[A, V](val rank: Int)(implicit int: Integral[A], hash: RTreeHash[A, V], bhash: RTreeHash[A, Border[A]]) {
  import RTreeHash.ValueOps

  private implicit val treeHash: RTreeHash[A, View[V]] = View.RTreeHash[A, V](hash)
  private implicit val borderHash: RTreeHash[A, View[Border[A]]] = View.RTreeHash[A, Border[A]](bhash)

  private var _loc: Pos[A] = Pos.zero[A](rank)
  private val _values: RTree[A, View[V]] = RTree.empty[A, View[V]](rank)
  private var _borders: RTree[A, View[Border[A]]] = RTree.empty[A, View[Border[A]]](rank)

  // private val added: mutable.HashSet[Box[A]] = mutable.HashSet.empty[Box[A]]
  // private val removed: mutable.HashSet[Box[A]] = mutable.HashSet.empty[Box[A]]
  private var changed: Boolean = false

  // Remove any parts of existing borders that overlap with a newly added box
  private def removeBordersForAddedBox(box: Box[A]): Unit = _borders(box).foreach{case view@View(border) =>
    _borders -= view
    _borders ++= (border diff box).map{b => View(b) }
  }

  // Remove any borders of a removed box if this part was on the outside
  private def removeBordersForRemovedBox(box: Box[A]): Unit = box.borders().foreach{border =>
    val borders = _borders(border.box)
    // Remove the original overlapping borders
    _borders --= borders
    // Add back anything that doesn't overlap with the part's border
    _borders ++= borders.flatMap{case View(b) => (b.box diff border.box).map{box => View(Border(b.dim, b.dir, box)) }}
  }

  // Add any new borders of an added box that don't overlap with other parts or existing borders
  private def addBordersForAddedBox(box: Box[A]): Unit = box.borders().foreach{border =>
    val add = border diff (_values(border.box).map(_.value.box) ++ _borders(border.box).map(_.value.box))
    _borders ++= add.map(View.apply)
  }

  // Add any borders of the neighbors of a removed box that don't overlap with other parts or existing borders
  private def addBordersForRemovedBox(box: Box[A]): Unit = box.borders().foreach{border =>
    val neighbors = _values(border.box).map(_.box)
    neighbors.foreach{neighbor => neighbor.borders().foreach{border =>
      val add = border diff (_values(border.box).map(_.box) ++ _borders(border.box).map(_.box))
      _borders ++= add.map(View.apply)
    }}
  }

  private def update(): Unit = if (changed) {
    _borders = RTree.empty[A, View[Border[A]]](rank)
    _borders ++= _values.iterator.flatMap{v => v.value.box.borders() }
      .flatMap{b => b diff _values.apply(b.box).map(_.value.box) }
      .map{border => View(border) }

    // added.foreach{ addBordersForAddedBox }
    // removed.foreach{ addBordersForRemovedBox }
    // removed.foreach{ removeBordersForRemovedBox }
    // added.foreach{ removeBordersForAddedBox }
    // added.clear()
    // removed.clear()
    changed = false
  }

  private def add(value: View[V]): Unit = {
    _values += value
    changed = true
    // if (removed.contains(value.box)) removed -= value.box else added += value.box
  }

  private def remove(value: View[V]): Unit = {
    _values -= value
    changed = true
    // if (added.contains(value.box)) added -= value.box else removed += value.box
  }

  object view {
    def iterator: Iterator[View[V]] = _values.iterator
    def apply(box: Box[A]): Set[View[V]] = _values.apply(box)
    def components(): Iterable[Set[View[V]]] = _values.components()
    def borders(): Iterator[View[Border[A]]] = { update(); _borders.iterator }
    def borders(predicate: Border[A] => Boolean): Iterator[View[Border[A]]] = borders().filter{v => predicate(v.value) }

    def -=(part: View[V]): Unit = remove(part)
    def +=(part: View[V]): Unit = add(part)
    def ++=(parts: IterableOnce[View[V]]): Unit = parts.iterator.foreach(add)
  }

  def moveto(pos: Pos[A]): Unit = { _loc = pos }

  def loc: Pos[A] = _loc
  def shape: Pos[A] = _values.shape
  def bbox: Box[A] = _values.bbox + _loc
  def size: Int = _values.size
  def depth: Int = _values.depth
  def nodes: Int = _values.nodes

  def iterator: Iterator[V] = _values.iterator.map(_.at(loc))
  def valueTree: RTree[A, View[V]] = _values

  def borders(): Iterator[Border[A]] = { update(); _borders.iterator.map(_.at(loc)) }
  def borders(predicate: Border[A] => Boolean): Iterator[Border[A]] = borders().filter(predicate)

  def hasValueAt(pos: Pos[A]): Boolean = _values.apply(pos - loc).nonEmpty
  def hasBorderAt(pos: Pos[A]): Boolean = { update(); _borders.apply(pos - loc).nonEmpty }

  // def +=(part: V): Unit = { view += View(part.move(-loc)) }
  // def ++=(parts: IterableOnce[V]): Unit = { parts.iterator.foreach{v => this += v } }
}

object BorderedRTree {
  def apply[A: Integral, V](rank: Int, loc: Pos[A], values: IterableOnce[View[V]])
                           (implicit hash: RTreeHash[A, V], bhash: RTreeHash[A, Border[A]]): BorderedRTree[A, V] = {
    val tree = new BorderedRTree[A, V](rank)
    tree.moveto(loc)
    tree.view ++= values
    tree
  }

  def one[A: Integral, V](value: V)
                         (implicit hash: RTreeHash[A, V], bhash: RTreeHash[A, Border[A]]): BorderedRTree[A, V] = {
    import RTreeHash.ValueOps
    val tree = new BorderedRTree[A, V](value.box.rank)
    tree.view += View(value)
    tree
  }
}
