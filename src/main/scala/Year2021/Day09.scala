package Year2021

import common.immutable.Pos

import scala.collection.mutable

object Day09 extends common.AoC(9, 2021) {
  // Padding because lazy
  val raw: Array[Array[Char]] = data.getLines().map { row => '9' +: row.toArray :+ '9' }.toArray
  val cols = raw(0).length
  val padding = Array.fill[Char](cols)('9')
  val map: Array[Array[Char]] = padding +: raw :+ padding
  map.foreach(row => println(row.mkString("")))
  val rows = map.length

  def part1(): Unit = {
    def isLowPoint(i: Int, j: Int): Boolean = {
      map(i - 1)(j) > map(i)(j) && map(i)(j - 1) > map(i)(j) &&
        map(i + 1)(j) > map(i)(j) && map(i)(j + 1) > map(i)(j)
    }

    val sum = (1 to rows - 2).foldLeft(0) { (accum, i) =>
      (1 to cols - 2).foldLeft(accum) { (accum, j) =>
        if (isLowPoint(i, j)) accum + map(i)(j) - '0' + 1 else accum
      }
    }

    println(s"Part 1: (Sum): $sum")
  }

  def part2(): Unit = {
    val basins = Seq.tabulate(rows) { _ => mutable.Seq.fill(cols)(-1) }
    var iter: Int = -1
    val sizes: mutable.Map[Int, Seq[Pos[Int]]] = mutable.Map.empty
    (0 until rows - 1).foreach { i =>
      (0 until cols - 1).foreach { j =>
        if (map(i)(j) < '9') {
          val up: Int = basins(i - 1)(j)
          val left: Int = basins(i)(j - 1)

          val label = if (up != -1 && left != -1 && up != left) {
            val removed = Math.max(up, left)
            val intersect = Math.min(up, left)
            sizes(removed).foreach { pos => basins(pos.r)(pos.c) = intersect }
            sizes(intersect) ++= sizes(removed)
            sizes(removed) = Nil
            intersect
          } else if (up != -1) {
            up
          } else if (left != -1) {
            left
          } else {
            iter = (iter + 1).toChar
            iter
          }
          basins(i)(j) = label
          sizes(label) = sizes.getOrElse(label, Nil) :+ Pos(i, j)
        }
      }
    }
    basins.foreach { line => println(line.map { c => if (c == -1) '!' else ('#' + (c % ('z' - '#'))).toChar }.mkString("")) }

    // Sanity check:
    def invalid(i: Int, j: Int, c: Int): Boolean = basins(i)(j) != -1 && basins(i)(j) != c

    (0 until rows).foreach { i =>
      (0 until cols).foreach { j =>
        val c = basins(i)(j)
        if (c != -1) {
          if (invalid(i - 1, j, c) || invalid(i + 1, j, c) || invalid(i, j - 1, c) || invalid(i, j + 1, c))
            println(s"Invalid at $i, $j")
        }
      }
    }
    val sorted = sizes.toList.map { case (label, pos) => label -> pos.length }.sortBy { x => -x._2 }
    sorted.take(6).foreach { case (label, len) =>
      println(s"$label: $len (${('#' + (label % ('z' - '#'))).toChar})")
    }
    val max3 = sizes.values.toSeq.map(_.length).sortBy(x => -x).take(3)
    println(s"Part 2: ${max3.mkString(" * ")} = ${max3.product}")
  }

  //part1()
  part2()
}
