package common.mutable

import common.immutable.{Box, Pos}
import common.traits.HasBox

import scala.collection.mutable

/**
 * Keeps a sparse N-dimensional space with ~O(log(S)) lookup from an N-D index or volume to one or more entries.
 */
class SparseGrid[A,V](val rank: Int)(implicit int: Integral[A], vol: HasBox[A,V]) {
  import HasBox._

  private type Entry = Either[Int,List[V]]

  private def floor(i: Pos[A], grid: Pos[A]): Pos[A] = (i / grid)*grid
  private def floor(c: Box[A], grid: Pos[A]): Box[A] = Box(floor(c.min, grid), floor(c.max, grid))

  case class Node(map: mutable.Map[Pos[A], Entry], grid: Pos[A]) {
    def iterate(volume: Box[A]): Iterator[Pos[A]] = floor(volume, grid).iteratorBy(grid)
    def update(i: Pos[A], index: Int): Unit = { map(i) = Left(index) }
    def update(i: Pos[A], entry: List[V]): Unit = { map(i) = Right(entry) }
    def get(i: Pos[A]): Entry = map.getOrElse(floor(i, grid), Left(-1))
  }
  object Node {
    def empty(grid: Pos[A]): Node = Node(mutable.HashMap.empty[Pos[A],Entry], grid)
    def create(volume: Box[A], values: List[V]): Node = {
      val grid: Pos[A] = values.map(_.box.shape).reduce(_ min _)
      val map = mutable.HashMap.empty[Pos[A], Entry] ++ values.flatMap{v =>
        v.box.intersect(volume).iterator.flatMap{ vol => floor(vol, grid).iteratorBy(grid).map{ pos => pos -> v }}
      }.groupMapReduce(_._1)(v => Right(List(v._2)) ){(a,b) => Right(a.value ++ b.value) }
        .filter{p => p._2.isLeft || p._2.value.nonEmpty }
      Node(map, grid)
    }
  }

  private var bounds: Box[A] = Box(Pos.zero[A](rank), Pos.zero[A](rank))
  private val nodes = mutable.ArrayBuffer.empty[Node]
  nodes += Node.empty(grid = Pos.fill[A](rank, int.fromInt(1024)))

  //def volume: Cube[A] = bounds
  def shape: Pos[A] = bounds.shape

  def get(i: Pos[A]): List[V] = LazyList.iterate(Left(0) : Entry){
    case Left(index) if index >= 0 && index < nodes.length => nodes(index).get(i)
    case Left(-1) => Right(Nil)
    case v => v
  }.dropWhile(_.isLeft).head.getOrElse(Nil)

  private def add(v: V): Unit = {
    bounds = bounds union v.box
    val worklist = mutable.Queue.empty[(Int, Box[A])]
    worklist.enqueue((0, v.box))
    while (worklist.nonEmpty) {
      val (idx, volume) = worklist.dequeue()
      val node = nodes(idx)
      node.iterate(volume).map{v => (v, node.get(v)) }.foreach{
        case (pos, Left(-1))  => node(pos) = List(v)
        case (pos, Left(idx)) =>
          val range = Box(pos, pos + (node.grid - int.one))
          v.box.intersect(range).foreach{ vol1 => worklist.enqueue((idx, vol1)) }
        case (pos, Right(existing)) =>
          val range = Box(pos, pos + (node.grid - int.one))
          node(pos) = nodes.length
          nodes += Node.create(range, v +: existing)
      }
    }
  }

  private def remove(v: V): Unit = {

  }

  def dump(): Unit = {
    val worklist = mutable.Stack.empty[(Int, Pos[A], Node)]
    worklist += ((0, Pos.zero[A](rank), nodes.head))
    while (worklist.nonEmpty) {
      val (indent, pos, current) = worklist.pop()
      println(s"${"  "*indent}$pos Grid ${current.grid}")
      current.map.foreach{
        case (k, Left(idx)) if idx >= 0 && idx < nodes.length => worklist += ((indent + 1, k, nodes(idx)))
        case (k, Left(idx)) => println(s"${"  "*(indent + 1)}$k: $idx")
        case (k, Right(vs)) => println(s"${"  "*(indent + 1)}$k: ${vs.mkString(", ")}")
      }
    }
  }

  def +=(v: V): Unit = add(v)
  def -=(v: V): Unit = remove(v)
}
object SparseGrid {
  def empty[A:Integral,V](rank: Int)(implicit v: HasBox[A,V]): SparseGrid[A,V] = new SparseGrid[A,V](rank)
}