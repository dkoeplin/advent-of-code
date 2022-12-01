package Year2021

import scala.collection.mutable

object Day12 extends App {
  val NodePattern = "([a-zA-Z]+)-([a-zA-Z]+)".r
  val file = scala.io.Source.fromFile("./data/12")
  val edges = file.getLines().map { case NodePattern(a, b) => (a, b) }.toArray
  val nodes: Array[String] = edges.flatMap { case (a, b) => Seq(a, b) }.distinct
  val small: Set[Int] = nodes.iterator.zipWithIndex.filter(_._1.head.isLower).map(_._2).toSet
  val names: Map[String, Int] = nodes.zipWithIndex.toMap
  val neighbors = mutable.Map.empty[Int, Set[Int]]

  object Node {
    def unapply(x: String): Option[Int] = Some(names(x))
  }

  edges.foreach { case (Node(a), Node(b)) =>
    neighbors(a) = neighbors.getOrElse(a, Set.empty) + b
    neighbors(b) = neighbors.getOrElse(b, Set.empty) + a
  }
  val start = names("start")
  val end = names("end")

  def dot(): Unit = {
    val out = new java.io.PrintWriter(new java.io.File("out/12.gv"))
    out.write("graph graphname {\n")
    nodes.foreach { case name@Node(n) =>
      out.write(s"  $name [label=$name, color=${if (small.contains(n)) "red" else "green"}];\n")
    }
    edges.foreach { case (a, b) =>
      out.write(s"  $a -- $b;\n")
    }
    out.write("}\n")
    out.close()
  }

  def dfs(part2: Boolean): Seq[Seq[Int]] = {
    val paths = mutable.Buffer.empty[Seq[Int]]
    val frontier = mutable.Buffer[(Int, Seq[Int])]((start, Nil))
    while (frontier.nonEmpty) {
      val (current, path) = frontier.last
      frontier.remove(frontier.size - 1)
      if (current == end) {
        paths += (path :+ end)
      } else if (path.count(_ == current) < 100) { // Just in case infinite loops are possible
        val counts = path.filter(small.contains).groupBy(x => x).mapValues(_.length)
        val small2 = counts.exists(_._2 >= 2)
        val blocked = if (part2 && !small2) Set(start) else path.toSet intersect small
        val next = neighbors(current) diff blocked
        //println(s"$current path: [${path.mkString("->")}] next: ${next.mkString(", ")}")
        if (!small.contains(current) || !small2 || counts.getOrElse(current, 0) < 1)
          frontier ++= next.map { i => (i, path :+ current) }
      }
    }
    paths
  }

  dot()
  val part1 = dfs(false)
  val part2 = dfs(true)
  part2.foreach { path => println(path.map {
    nodes.apply
  }.mkString(","))
  }
  println(s"Part 1: ${part1.size}")
  println(s"Part 2: ${part2.size}")
}
