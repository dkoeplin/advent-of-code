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
  private val _tree: RTree[A, View[V]] = RTree.empty[A, View[V]](rank)
  private val _borders: RTree[A, View[Border[A]]] = RTree.empty[A, View[Border[A]]](rank)

  private def remove(part: View[V]): Unit = {
    _tree -= part
    // Remove any borders of this part if this part was on the outside
    part.box.borders().foreach { border =>
      val borders = _borders(border.box)
      _borders --= borders // Remove the original overlapping borders
      val remain = borders.flatMap { case View(b) => (b.box diff border.box).map { box => View(Border(b.dim, b.dir, box)) } }
      _borders ++= remain // Add back anything that doesn't overlap with the part's border
    }
    // Add any new borders of parts remaining that bordered with this removed part
    part.box.borders().foreach { border =>
      val neighbors = _tree(border.box)
      val borders = neighbors.map { n => n.box.border(border.dim, -border.dir) }
      borders.foreach { border =>
        val overlap = _tree(border.box).map(_.box)
        val remain = border.box diff overlap
        _borders ++= remain.map { b => View(Border(border.dim, border.dir, b)) }
      }
    }
  }

  object view {
    private def add(part: View[V]): Unit = {
      _tree += part
      // Add any borders of the new part that don't overlap with existing other parts
      part.box.borders().foreach { border =>
        val overlap = _tree(border.box).map(_.box)
        val remain = border.box diff overlap
        _borders ++= remain.map { b => View(Border(border.dim, border.dir, b)) }
      }
      // Remove any parts of existing borders that overlap with this new part
      _borders(part.box).foreach { case view@View(border) =>
        val remain = border.box diff part.box
        _borders -= view
        _borders ++= remain.map { b => View(Border(border.dim, border.dir, b)) }
      }
    }

    def iterator: Iterator[View[V]] = _tree.iterator

    def apply(box: Box[A]): Set[View[V]] = _tree.apply(box)

    def components(): Iterable[Set[View[V]]] = _tree.components()

    def borders(): Iterator[View[Border[A]]] = _borders.iterator
    def borders(predicate: Border[A] => Boolean): Iterator[View[Border[A]]] = view.borders().filter { v => predicate(v.value) }

    def -=(part: View[V]): Unit = remove(part)
    def +=(part: View[V]): Unit = add(part)
    def ++=(parts: IterableOnce[View[V]]): Unit = parts.iterator.foreach(add)
  }

  def moveto(pos: Pos[A]): Unit = { _loc = pos }

  def loc: Pos[A] = _loc
  def shape: Pos[A] = _tree.shape
  def bbox: Box[A] = _tree.bbox + _loc
  def size: Int = _tree.size
  def depth: Int = _tree.depth
  def nodes: Int = _tree.nodes

  def iterator: Iterator[V] = _tree.iterator.map(_.at(loc))
  def valueTree: RTree[A, View[V]] = _tree

  def borders(): Iterator[Border[A]] = _borders.iterator.map(_.at(loc))
  def borders(predicate: Border[A] => Boolean): Iterator[Border[A]] = borders().filter(predicate)

  def +=(part: V): Unit = { view += View(part.move(-loc)) }
  def ++=(parts: IterableOnce[V]): Unit = { parts.iterator.foreach{v => this += v } }
}

object BorderedRTree {
  def apply[A: Integral, V](rank: Int, loc: Pos[A], values: IterableOnce[View[V]])(implicit hash: RTreeHash[A, V], bhash: RTreeHash[A, Border[A]]): BorderedRTree[A, V] = {
    val tree = new BorderedRTree[A, V](rank)
    tree.moveto(loc)
    tree.view ++= values
    tree
  }

  def one[A: Integral, V](value: V)(implicit hash: RTreeHash[A, V], bhash: RTreeHash[A, Border[A]]): BorderedRTree[A, V] = {
    import RTreeHash.ValueOps
    val tree = new BorderedRTree[A, V](value.box.rank)
    tree += value
    tree
  }
}
