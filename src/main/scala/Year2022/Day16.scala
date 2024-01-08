package Year2022

object Day16 extends common.AoC(16, 2022) {
  case class Node(name: String, rate: Int, neighbors: Array[String])
  object Node {
    private val pattern = "Valve ([A-Z][A-Z]) has flow rate=([0-9]+); tunnels? leads? to valves? (.*)".r
    def parse(line: String): Node = pattern.findFirstMatchIn(line).map{m =>
      val name = m.group(1)
      val rate = m.group(2).toInt
      val edges = m.group(3).split(", ")
      Node(name, rate, edges)
    }.getOrElse(throw new Exception(s"Unable to parse $line"))
  }

  val graph: Map[String, Node] = data.getLines().map(Node.parse).toArray.groupBy(_.name).view.mapValues(_.head).toMap
  val distances = scala.collection.mutable.Map.empty[(String, String), Seq[String]]
  def path(a: String, b: String): Seq[String] = distances.getOrElse((a,b), {
    case class Vertex(node: String, dist: Int)
    implicit def ordering: Ordering[Vertex] = (a: Vertex, b: Vertex) => implicitly[Ordering[Int]].compare(-a.dist, -b.dist)

    val queue = scala.collection.mutable.PriorityQueue.empty[Vertex]
    val dist = scala.collection.mutable.Map.empty[String, Int]
    val prev = scala.collection.mutable.Map.empty[String, String]
    val visited = scala.collection.mutable.Set.empty[String]
    def distance(x: String): Int = dist.getOrElse(x, Int.MaxValue)
    queue.enqueue(Vertex(a, 0))
    dist(a) = 0
    while (queue.nonEmpty) {
      val v = queue.dequeue()
      if (!visited.contains(v.node)) {
        visited += v.node
        graph(v.node).neighbors.foreach{ next =>
          val total = distance(v.node) + 1
          if (total < distance(next)) {
            dist(next) = total
            prev(next) = v.node
            queue.enqueue(Vertex(next, total))
          }
        }
      }
    }
    dist.foreach{case (dest, len) =>
      var path: List[String] = List(dest)
      while (path.head != a) {
        path = prev(path.head) +: path
      }
      distances((a, dest)) = path
    }
    distances((a,b))
  })

  val candidates: Set[String] = graph.keySet.filter{ n => graph(n).rate > 0 }
  println(s"Nodes > 0: ${candidates.size} ${candidates.mkString(" ")}")

  case class Step(nodes: Seq[String])
  case class Path(nodes: Array[Step], t: Seq[Int], total: Int, MINUTES: Int) {
    def unopened: Seq[String] = (candidates diff nodes.flatMap(_.nodes).toSet).toSeq
    def prev(i: Int): String = nodes.lastOption.map(_.nodes.apply(i)).getOrElse("AA")
    def add(step: Step): Path = {
      val rates = step.nodes.zipWithIndex.map{case (node, i) =>
        if (node.nonEmpty) {
          val dist: Int = t(i) + path(prev(i), node).length
          val rate = (MINUTES - dist + 1) * graph(node).rate
          (dist, rate)
        } else (0, 0)
      }
      val inc = rates.map(_._2).sum
      Path(nodes :+ step, rates.map(_._1), total + inc, MINUTES)
    }
    def next(n: Int): Iterator[Path] = {
      (unopened ++ Seq.fill(n)("AA")).combinations(n).flatMap{nodes =>
        nodes.permutations.map{step => add(Step(step)) }
      }
    }
    def valid: Boolean = t.forall(_ <= MINUTES)
    def stop: Boolean = nodes.last.nodes.forall(_ == "AA")
  }
  object Path {
    def initial(n: Int, MINUTES: Int): Path = Path(Array.empty, Seq.fill(n)(1), 0, MINUTES)
  }

  def path(p: Int, n: Int, MINUTES: Int): Unit = {
    println(s"== Part $p ==")
    var length: Int = 1
    var best: Option[Path] = None
    var worklist: Array[Path] = Array(Path.initial(n, MINUTES))
    while (worklist.nonEmpty) {
      val next = worklist.flatMap{p => p.next(n) }.groupBy(_.nodes.flatMap(_.nodes).toSet)
      val tops = next.view.mapValues(_.maxBy(_.total)).values
      val top = tops.maxByOption(_.total)
      worklist = tops.filterNot(_.stop).toArray
      best = top.filter{t => best.isEmpty || best.get.total < t.total }.orElse(best)

      if (top.nonEmpty) {
        println(s"Length $length: ${top.get.total} (next: ${worklist.length})")
        (0 until n).foreach{i =>
          println(s"  ${top.get.nodes.map(_.nodes(i)).mkString(" ")}")
        }
      } else {
        println(s"Length $length: No possible paths")
      }
      length += 1
    }
    best.foreach { part =>
      println(s"Part$p: ${part.total}")
      (0 until n).foreach{i =>
        val nodes = part.nodes.map(_.nodes.apply(i))
        println(s"  ${nodes.mkString(" ")}")
      }
    }
  }

  path(p=1, n=1, MINUTES=30)
  path(p=2, n=2, MINUTES=26)
}
