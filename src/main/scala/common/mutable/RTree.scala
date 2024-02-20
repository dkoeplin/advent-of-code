package common.mutable

import common.immutable.{Box, Pos}
import common.traits.RTreeHash

import scala.collection.mutable

/**
 * An unbalanced / variable granularity variation of an R-Tree
 * Keeps a sparse N-dimensional space with ~O(log(S)) lookup from an N-D index or volume to one or more entries.
 */
class RTree[A,V](val rank: Int, private val kMaxEntries: Int = 10)(implicit int: Integral[A], vol: RTreeHash[A,V]) {
  import RTree.{clamp, clampDown}
  import RTreeHash._
  import int._

  private type Entry = Either[Node,Set[V]]

  private class Node(val parent: Option[(Node, Pos[A])], val map: mutable.Map[Pos[A], Entry], val grid: Pos[A]) {
    def iterateOver(box: Box[A]): Iterator[Pos[A]] = {
      val volume = clamp(box, grid)
      volume.posIterator(grid)
    }

    def update(i: Pos[A], entry: Entry): Unit = entry match {
      case Right(set) if set.isEmpty => map.remove(i)
      case Left(Null) => map.remove(i)
      case _ => map(i) = entry
    }

    def get(i: Pos[A])(implicit int: Integral[A]): Entry = map.getOrElse(clampDown(i, grid), Left(Null))

    def remove(i: Pos[A]): Unit = map.remove(i)
  }
  private case object Null extends Node(None, null, null)
  private object Node {
    def empty(grid: Pos[A]): Node = new Node(None, mutable.HashMap.empty[Pos[A],Entry], grid)
  }

  private def increaseDepth(parent: Node, values: Set[_]): Boolean
    = values.size >= kMaxEntries && parent.grid.iterator.forall(_ > kGridMin)

  private def makeNode(parent: Node, pos: Pos[A], values: Set[V]): Node = {
    val grid: Pos[A] = parent.grid / kGridBase
    val range = Box(pos, pos + parent.grid - int.one) // Entire range now covered by this map
    val map = mutable.HashMap.empty[Pos[A], Entry]
    values.foreach{value =>
      value.box.intersect(range).foreach{intersect => // Area of this entry overlapping with this map
        // println(s"[B]  Iterating over ${clamp(vol, grid)} for $vol on grid $grid")
        clamp(intersect, grid).boxIterator(grid).foreach{range =>
          if (range.overlaps(value.box)) {
            val prev: Set[V] = map.getOrElse(range.min, Right(Set.empty[V])).getOrElse(Set.empty[V])
            // println(s"[B]    Adding $v at $spot")
            map(range.min) = Right(prev + value)
          }
        }
      }
    }
    new Node(Some((parent, pos)), map, grid)
  }

  private def createEntry(rootParent: Node, pos: Pos[A], values: Set[V]): Entry
  = if (!increaseDepth(rootParent, values)) Right(values) else {
    case class Work(parent: Node, pos: Pos[A])

    val root = makeNode(rootParent, pos, values)
    var worklist: List[Work] = List(Work(root, pos))
    while (worklist.nonEmpty) {
      val Work(parent, pos) = worklist.head
      worklist = worklist.tail
      parent.get(pos) match {
        case Right(values) if increaseDepth(parent, values) =>
          val child = makeNode(parent, pos, values)
          if (child.map.nonEmpty) {
            parent(pos) = Left(child)
            worklist = Work(parent, pos) +: worklist
          } else {
            parent.remove(pos)
          }
        case Right(_)   => // Leave this for now
        case Left(Null) => // Nothing to do (also this shouldn't happen?)
        case Left(node) =>
          worklist = worklist ++ node.map.keysIterator.map{k => Work(node, k) }
      }
    }
    Left(root)
  }

  private class Worklist {
    def +=(node: Node): Unit = { nodes += node }
    def ++=(node: Option[Node]): Unit = { nodes ++= node }
    def cleanup(): Unit = {
      while (nodes.nonEmpty) {
        val node = nodes.head
        nodes = nodes.tail
        if (node.map.isEmpty) node.parent match {
          case Some((parent, pos)) =>
            parent.map.remove(pos) // Removes the current node from the tree
            nodes += parent     // Check the parent for removal
          case None => // Do nothing, never delete the root
        }
      }
    }
    var nodes: mutable.LinkedHashSet[Node] = mutable.LinkedHashSet.empty[Node]
  }

  /// Preorder traversal
  def preorder(preorder: (Int, Box[A], Pos[A]) => Unit)(func: (Int, Box[A], Set[V]) => Unit): Unit = {
    val worklist = mutable.Stack.empty[(Int, Node)]
    worklist += ((0, root))
    while (worklist.nonEmpty) {
      val (depth, current) = worklist.pop()
      val pos: Pos[A] = current.parent.map(_._2).getOrElse(Pos.zero[A](rank))
      val end: Pos[A] = pos + current.parent.map(_._1.grid).getOrElse(Pos.zero[A](rank)) - int.one
      if (current.map.isEmpty) println(s"${"  "*depth} EMPTY MAP")
      if (depth > 0) preorder(depth, Box(pos, end), current.grid)
      current.map.foreach {
        case (k, Left(child)) => worklist += ((depth + 1, child))
        case (k, Right(vs)) =>
          val box = Box(k, k + current.grid - int.one)
          func(depth, box, vs)
      }
    }
  }
  def iterate(func: (Int, Box[A], Set[V]) => Unit): Unit = preorder{(_,_,_) => ()}(func)

  /// Traverse all entries, including those that are not yet defined.
  private def traverseAll(v: Box[A])(func: (Node, Pos[A], Set[V]) => Unit): Unit = {
    case class Work(node: Node, vol: Box[A])
    var worklist: List[Work] = List(Work(root, v))
    while (worklist.nonEmpty) {
      val Work(node, volume) = worklist.head
      worklist = worklist.tail
      node.iterateOver(volume).map{ pos => (pos, node.get(pos)) }.foreach{
        case (pos, Right(entries)) => func(node, pos, entries)
        case (pos, Left(Null))     => func(node, pos, Set.empty)
        case (pos, Left(child))    =>
          val range = Box(pos, pos + node.grid - int.one)
          worklist = worklist ++ v.intersect(range).map{vol => Work(child, vol) }
      }
    }
  }

  /// Traverse only defined boxes
  private def traverseDefined(v: Box[A])(func: (Node, Pos[A], Set[V]) => Unit): Unit = {
    case class Work(node: Node, vol: Box[A])
    var worklist: List[Work] = List(Work(root, v))
    while (worklist.nonEmpty) {
      val Work(node, volume) = worklist.head
      worklist = worklist.tail
      node.iterateOver(volume).map{pos => (pos, node.get(pos)) }.foreach{
        case (pos, Right(entries)) => func(node, pos, entries)
        case (pos, Left(Null))     => // Do nothing
        case (pos, Left(child))    =>
          val range = Box(pos, pos + node.grid - int.one)
          worklist = worklist ++ v.intersect(range).map{vol => Work(child, vol) }
      }
    }
  }

  private def addAt(v: V, box: Box[A]): Unit = {
    // println(s"---\nAdding $v")
    bounds = Some(bounds.map(_ union box).getOrElse(box))
    traverseAll(box){(node, pos, entries) => node(pos) = createEntry(node, pos, entries + v) }
  }

  private def add(v: V): Unit = {
    entries(v.hash) = v
    addAt(v, v.box)
  }

  private def removeAt(v: V, node: Node, pos: Pos[A], entries: Set[V]): Option[Node] = {
    if (entries.size == 1) {
      node.map.remove(pos)
      if (node.map.isEmpty) Some(node) else None
    } else {
      node(pos) = Right(entries - v)
      None
    }
  }

  private def removeFrom(v: V, box: Box[A]): Unit = {
    val worklist = new Worklist
    traverseDefined(box){(node, pos, entries) => worklist ++= removeAt(v, node, pos, entries) }
    worklist.cleanup()
  }

  private def remove(v: V): Unit = {
    entries.remove(v.hash)
    removeFrom(v, v.box)
  }

  def dump(out: String => Unit = println): Unit = {
    preorder{(lvl, box, gd) => out(s"${"  " * lvl}$box: Grid $gd") }
            {(lvl, box, vs) => out(s"${"  " * (lvl + 1)}$box: ${vs.mkString(", ")}") }
  }

  def +=(v: V): Unit = add(v)
  def ++=(v: IterableOnce[V]): Unit = v.iterator.foreach(add)

  def -=(v: V): Unit = remove(v)
  def --=(v: IterableOnce[V]): Unit = v.iterator.foreach(remove)

  def apply(i: Box[A]): Set[V] = {
    val set = mutable.LinkedHashSet.empty[V]
    traverseDefined(i) { (_, _, entries) => set ++= entries.filter(_.box.overlaps(i)) }
    set.toSet
  }
  def apply(i: Pos[A]): Set[V] = apply(Box(i, i))

  def bbox: Box[A] = bounds.getOrElse(Box.unit(Pos.zero[A](rank)))
  def shape: Pos[A] = bounds.map(_.shape).getOrElse(Pos.zero[A](rank))
  def size: Int = entries.size
  def iterator: Iterator[V] = entries.valuesIterator

  def moveEntry(v: V, prev: Box[A]): Unit = {
    val worklist = new Worklist
    (prev diff v.box).foreach{box =>
      traverseDefined(box){(node, pos, entries) =>
        val grid = Box(pos, pos + node.grid - int.one)
        if (!v.box.overlaps(grid)) worklist ++= removeAt(v, node, pos, entries)
      }
    }
    (v.box diff prev).foreach{box => addAt(v, box) }
    worklist.cleanup()
  }

  /// Debug info
  def depth: Int = {
    var maxDepth: Int = 1
    iterate{(lvl, _, _) => maxDepth = Math.max(lvl + 1, maxDepth) }
    maxDepth
  }
  def nodes: Int = {
    var nodes: Int = 1
    preorder{(_,_,_) => nodes += 1}{(_,_,_) => ()}
    nodes
  }

  def get(key: A): Option[V] = entries.get(key)

  def components(): Iterable[Set[V]] = {
    var id: Int = 0
    val componentMap = mutable.HashMap.empty[V, Int]
    val equivalent = common.mutable.EquivalentSets.empty[Int]
    for (entry <- entries.values) {
      val neighbors = entry.box.borders().flatMap{border => apply(border.box) }.toSet
      val components = neighbors.flatMap{ componentMap.get }
      val component = components.headOption.getOrElse{ id += 1; id}
      componentMap(entry) = component
      equivalent.add(components + component)
    }
    componentMap.view.groupMapReduce{case (_, k) => equivalent(k) }{case (v, _) => Set(v) }{_ ++ _}.values
  }

  private val kGridBase: A = int.fromInt(2)
  private val kGridMin: A = int.fromInt(2)

  private var bounds: Option[Box[A]] = None
  private val entries = mutable.LinkedHashMap.empty[A,V]
  private val root = Node.empty(grid = Pos.fill[A](rank, int.fromInt(1024)))
}
object RTree {
  def empty[A:Integral,V](rank: Int, maxEntries: Int = 10)(implicit v: RTreeHash[A,V]): RTree[A,V] = new RTree[A,V](rank, maxEntries)
  def from[A:Integral,V](rank: Int, entries: IterableOnce[V], maxEntries: Int = 10)(implicit v: RTreeHash[A,V]): RTree[A,V] = {
    val tree = new RTree(rank, maxEntries)
    entries.iterator.foreach{entry => tree += entry}
    tree
  }
  def single[A:Integral,V](entry: V, maxEntries: Int = 10)(implicit v: RTreeHash[A,V]): RTree[A,V] = {
    val tree = new RTree(v.box(entry).rank, maxEntries)
    tree += entry
    tree
  }

  def clampUp[A](i: A, grid: A)(implicit int: Integral[A]): A = {
    import int._
    if (int.lt(i, int.zero)) (i/grid)*grid - int.one else ((i + grid)/grid)*grid - int.one
  }
  def clampDown[A](i: A, grid: A)(implicit int: Integral[A]): A = {
    import int._
    if (int.lt(i, int.zero)) ((i - grid + int.one) / grid)*grid else (i / grid)*grid
  }

  def clampUp[A:Integral](i: Pos[A], grid: Pos[A]): Pos[A] = i.zip(grid){(a,b) => clampUp(a,b) }
  def clampDown[A:Integral](i: Pos[A], grid: Pos[A]): Pos[A] = i.zip(grid){(a,b) => clampDown(a,b) }
  def clamp[A:Integral](c: Box[A], grid: Pos[A]): Box[A] = Box(clampDown(c.min, grid), clampUp(c.max, grid))
}