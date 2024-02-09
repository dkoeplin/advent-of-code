package common.mutable

import common.immutable.{Box, Pos}
import common.traits.HasBox

import scala.collection.mutable

/**
 * An unbalanced / variable granularity variation of an R-Tree
 * Keeps a sparse N-dimensional space with ~O(log(S)) lookup from an N-D index or volume to one or more entries.
 */
class RTree[A,V](val rank: Int, private val kMaxEntries: Int = 10)(implicit int: Integral[A], vol: HasBox[A,V]) {
  import HasBox._
  import RTree.{clamp, floor}
  import int._
6
  private val kGridBase: A = int.fromInt(2)
  private val kGridMin: A = int.fromInt(2)

  private type Entry = Either[Node,Set[V]]
  private object Entry {
    def increaseDepth(parent: Node, values: Set[V]): Boolean = {
      values.size >= kMaxEntries && parent.grid.iterator.forall(_ > kGridMin)
    }
    private def make(parent: Node, pos: Pos[A], values: Set[V]): Node = {
      val grid: Pos[A] = parent.grid / kGridBase
      val range = Box(pos, pos + parent.grid - int.one) // Entire range now covered by this map
      val map = mutable.HashMap.empty[Pos[A], Entry]
      values.foreach{value =>
        value.box.intersect(range).foreach{intersect => // Area of this entry overlapping with this map
          // println(s"[B]  Iterating over ${clamp(vol, grid)} for $vol on grid $grid")
          clamp(intersect, grid).iteratorBy(grid).foreach{spot =>
            if (value.box.contains(spot)) {
              val prev: Set[V] = map.getOrElse(spot, Right(Set.empty[V])).getOrElse(Set.empty[V])
              // println(s"[B]    Adding $v at $spot")
              map(spot) = Right(prev + value)
            }
          }
        }
      }
      new Node(Some((parent, pos)), map, grid)
    }

    def create(rootParent: Node, pos: Pos[A], values: Set[V]): Entry = if (!increaseDepth(rootParent, values)) Right(values) else {
      case class Work(parent: Node, pos: Pos[A])

      val root = make(rootParent, pos, values)
      var worklist: List[Work] = List(Work(root, pos))
      while (worklist.nonEmpty) {
        val Work(parent, pos) = worklist.head
        worklist = worklist.tail
        parent.get(pos) match {
          case Right(values) if increaseDepth(parent, values) =>
            val child = make(parent, pos, values)
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
  }

  private class Worklist {
    def +=(node: Node): Unit = { nodes += node }
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

  private class Node(val parent: Option[(Node, Pos[A])], val map: mutable.Map[Pos[A], Entry], val grid: Pos[A]) {
    def iterate(volume: Box[A]): Iterator[Pos[A]] = clamp(volume, grid).iteratorBy(grid)

    def update(i: Pos[A], entry: Entry): Unit = entry match {
      case Right(set) if set.isEmpty => map.remove(i)
      case Left(Null) => map.remove(i)
      case _ => map(i) = entry
    }

    def get(i: Pos[A]): Entry = map.getOrElse(floor(i, grid), Left(Null))

    def remove(i: Pos[A]): Unit = map.remove(i)
  }

  private case object Null extends Node(None, null, null)
  private object Node {
    def empty(grid: Pos[A]): Node = new Node(None, mutable.HashMap.empty[Pos[A],Entry], grid)
  }

  private case class Visit(node: Node, pos: Pos[A], entries: Set[V])
  private def traverse(v: Box[A])(func: (Node, Pos[A], Set[V]) => Unit): Unit = {
    case class Work(node: Node, vol: Box[A])
    var worklist: List[Work] = List(Work(root, v))
    while (worklist.nonEmpty) {
      val Work(node, volume) = worklist.head
      worklist = worklist.tail
      // println(s"[T] Iterating on grid ${node.grid} over $volume")
      node.iterate(volume).map{pos => (pos, node.get(pos)) }.foreach{
        case (pos, Right(entries)) => func(node, pos, entries)
        case (pos, Left(Null))     => func(node, pos, Set.empty)
        case (pos, Left(child))    =>
          val range = Box(pos, pos + node.grid - int.one)
          // println(s"[T]  Iterating over child range $range - ${v.intersect(range)}")
          worklist = worklist ++ v.intersect(range).map{vol => Work(child, vol) }
      }
    }
  }

  private def addAt(v: V, box: Box[A]): Unit = {
    // println(s"---\nAdding $v")
    bounds = Some(bounds.map(_ union box).getOrElse(box))
    traverse(box){(node, pos, entries) =>
      // println(s"Adding $v to grid ${node.grid} at $pos")
      node(pos) = Entry.create(node, pos, entries + v)
    }
  }

  private def add(v: V): Unit = {
    entries += v
    addAt(v, v.box)
  }

  private def removeFrom(v: V, box: Box[A]): Unit = {
    val worklist = new Worklist
    traverse(box){(node, pos, entries) =>
      if (entries.size == 1) {
        node.map.remove(pos)
        if (node.map.isEmpty)
          worklist += node
      } else {
        node(pos) = Right(entries - v)
      }
    }
    worklist.cleanup()
  }

  private def remove(v: V): Unit = {
    entries -= v
    removeFrom(v, v.box)
  }

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

  def dump(): Unit = preorder{case (lvl, box, gd) => println(s"${"  "*lvl}$box: Grid $gd") }
                             {case (lvl, box, vs) => println(s"${"  "*(lvl + 1)}$box: ${vs.mkString(", ")}") }

  def +=(v: V): Unit = add(v)
  def ++=(v: IterableOnce[V]): Unit = v.iterator.foreach(add)

  def -=(v: V): Unit = remove(v)
  def --=(v: IterableOnce[V]): Unit = v.iterator.foreach(remove)

  def apply(i: Box[A]): Set[V] = {
    val set = mutable.LinkedHashSet.empty[V]
    traverse(i - offset){(_, _, entries) => set ++= entries }
    set.toSet
  }
  def apply(i: Pos[A]): Set[V] = apply(Box(i, i))

  def loc: Pos[A] = offset
  def bbox: Box[A] = bounds.getOrElse(Box.unit(Pos.zero[A](rank))) + offset
  def shape: Pos[A] = bounds.map(_.shape).getOrElse(Pos.zero[A](rank))
  def size: Int = entries.size
  def iterator: Iterator[V] = entries.iterator
  def move(delta: Pos[A]): Unit = { offset += delta }
  def moveto(pos: Pos[A]): Unit = { offset = pos }

  def moveEntry(v: V, prev: Box[A]): Unit = {
    removeFrom(v, prev)
    addAt(v, v.box)
  }

  /// Debug info
  def depth: Int = {
    var maxDepth: Int = 1
    iterate{case (lvl, _, _) => maxDepth = Math.max(lvl + 1, maxDepth) }
    maxDepth
  }
  def nodes: Int = {
    var nodes: Int = 1
    preorder{case (_,_,_) => nodes += 1}{case (_,_,_) => ()}
    nodes
  }

  private var offset: Pos[A] = Pos.zero[A](rank)
  private var bounds: Option[Box[A]] = None
  private val entries = mutable.LinkedHashSet.empty[V]
  private val root = Node.empty(grid = Pos.fill[A](rank, int.fromInt(1024)))
}
object RTree {
  def empty[A:Integral,V](rank: Int, maxEntries: Int = 10)(implicit v: HasBox[A,V]): RTree[A,V] = new RTree[A,V](rank, maxEntries)
  def from[A:Integral,V](rank: Int, entries: IterableOnce[V], maxEntries: Int = 10)(implicit v: HasBox[A,V]): RTree[A,V] = {
    val tree = new RTree(rank, maxEntries)
    entries.iterator.foreach{entry => tree += entry}
    tree
  }
  def single[A:Integral,V](entry: V, maxEntries: Int = 10)(implicit v: HasBox[A,V]): RTree[A,V] = {
    val tree = new RTree(v.box(entry).rank, maxEntries)
    tree += entry
    tree
  }

  def ceil[A:Integral](i: Pos[A], grid: Pos[A]): Pos[A] = ((i + grid)/grid)*grid
  def floor[A:Integral](i: Pos[A], grid: Pos[A]): Pos[A] = (i / grid)*grid
  def clamp[A:Integral](c: Box[A], grid: Pos[A]): Box[A] = Box(floor(c.min, grid), ceil(c.max, grid) - implicitly[Integral[A]].one)
}