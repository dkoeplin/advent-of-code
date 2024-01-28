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

  private type Entry = Either[Node,Set[V]]

  private def floor(i: Pos[A], grid: Pos[A]): Pos[A] = (i / grid)*grid
  private def floor(c: Box[A], grid: Pos[A]): Box[A] = Box(floor(c.min, grid), floor(c.max, grid))

  private class Node(val parent: Option[(Node, Pos[A])], val map: mutable.Map[Pos[A], Entry], val grid: Pos[A]) {
    def iterate(volume: Box[A]): Iterator[Pos[A]] = floor(volume, grid).iteratorBy(grid)
    def update(i: Pos[A], entry: Entry): Unit = { map(i) = entry }
    def get(i: Pos[A]): Entry = map.getOrElse(floor(i, grid), Left(Null))
  }
  private case object Null extends Node(None, null, null)
  private object Node {
    def empty(grid: Pos[A]): Node = new Node(None, mutable.HashMap.empty[Pos[A],Entry], grid)
    def create(parent: (Node, Pos[A]), volume: Box[A], values: Set[V]): Node = {
      val grid: Pos[A] = values.map(_.box.shape).reduce(_ min _)
      val map = mutable.HashMap.empty[Pos[A], Entry] ++ values.flatMap{v =>
        v.box.intersect(volume).iterator.flatMap{ vol => floor(vol, grid).iteratorBy(grid).map{pos => pos -> v }}
      }.groupMapReduce(_._1)(v => Right(Set(v._2)) ){(a,b) => Right(a.value ++ b.value) }
        .filter{p => p._2.isLeft || p._2.value.nonEmpty }
      new Node(Some(parent), map, grid)
    }
  }

  private case class Visit(node: Node, pos: Pos[A], entries: Set[V])
  private def traverse(v: Box[A]): List[Visit] = {
    case class Work(node: Node, vol: Box[A])
    Iterator.iterate((List(Work(root, v)), List.empty[Visit])){case (worklist, visits) =>
      val Work(node, volume) = worklist.head
      node.iterate(volume).map{v => (v, node.get(v)) }.foldLeft((worklist.tail, visits)){
        case ((worklist, visits), entry) => entry match {
          case (pos, Right(entries)) => (worklist, Visit(node, pos, entries) +: visits)
          case (pos, Left(Null))     => (worklist, Visit(node, pos, Set.empty) +: visits)
          case (pos, Left(child))    =>
            val range = Box(pos, pos + (node.grid - int.one))
            (v.intersect(range).map{vol => Work(child, vol)}.toList ++ worklist, visits)
        }
      }
    }.dropWhile(_._1.nonEmpty).next()._2
  }

  private def add(v: V): Unit = {
    bounds = bounds union v.box
    entries += v
    traverse(v.box).foreach{case Visit(node, pos, entries) =>
      if (entries.size + 1 < kMaxEntries) {
        node(pos) = Right(entries + v)
      } else {
        val range = Box(pos, pos + (node.grid - int.one))
        node(pos) = Left(Node.create((node, pos), range, entries + v))
      }
    }
  }

  private def remove(v: V): Unit = {
    entries -= v
    var worklist = mutable.LinkedHashSet.empty[Node]
    traverse(v.box).foreach{case Visit(node, pos, entries) =>
      if (entries.size == 1) {
        node.map.remove(pos)
        if (node.map.isEmpty)
          worklist += node
      } else {
        node(pos) = Right(entries - v)
      }
    }
    while (worklist.nonEmpty) {
      val node = worklist.head
      worklist = worklist.tail
      if (node.map.isEmpty) node.parent match {
        case Some((parent, pos)) =>
          parent.map.remove(pos) // Removes the current node from the tree
          worklist += parent     // Check the parent for removal
        case None => // Do nothing, never delete the root
      }
    }
  }

  def dump(): Unit = {
    val worklist = mutable.Stack.empty[(Int, Node)]
    worklist += ((0, root))
    while (worklist.nonEmpty) {
      val (indent, current) = worklist.pop()
      val pos: Pos[A] = current.parent.map(_._2).getOrElse(Pos.zero[A](rank))
      val end: Pos[A] = pos + current.parent.map(_._1.grid).getOrElse(Pos.zero[A](rank)) - int.one
      println(s"${"  "*indent}$pos to $end: Grid ${current.grid}")
      current.map.foreach{
        case (k, Left(child)) => worklist += ((indent + 1, child))
        case (k, Right(vs))   => println(s"${"  "*(indent + 1)}$k: ${vs.mkString(", ")}")
      }
    }
  }

  def +=(v: V): Unit = add(v)
  def ++=(v: IterableOnce[V]): Unit = v.iterator.foreach(add)

  def -=(v: V): Unit = remove(v)
  def --=(v: IterableOnce[V]): Unit = v.iterator.foreach(remove)

  def apply(i: Box[A]): List[V] = traverse(i).flatMap(_.entries)
  def apply(i: Pos[A]): List[V] = traverse(Box(i, i)).flatMap(_.entries)

  def bbox: Box[A] = bounds
  def shape: Pos[A] = bounds.shape
  def size: Int = entries.size
  def iterator: Iterator[V] = entries.iterator
  def foreach(func: V => Unit): Unit = entries.foreach(func)

  private var bounds: Box[A] = Box(Pos.zero[A](rank), Pos.zero[A](rank))
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
}