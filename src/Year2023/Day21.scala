package Year2023

import common.Pos
import common.math
import common.immutable.Matrix
import common.algorithm.Dijkstra

object Day21 extends Year2023(21) {
  class GardenMap(iter: Iterator[Iterable[Char]]) extends Matrix[Char](iter) {
    lazy val start: Pos = posIterator().find { p => apply(p) == 'S' }.get
    def next(p: Pos): Iterator[Pos]
      = Pos.nondiag.iterator.map(_ + p).filter { p => get(p).exists { c => c == '.' || c == 'S' } }

    def reachable(begin: Pos, n: Int): Set[Pos] = (1 to n).foldLeft(Set(begin)){(f,_) => f.flatMap(next) }

    def reachableAt(n: Int): Unit = {
      val stepsIsOdd = n%2 == 0
      val tiles = posIterator().filter{p => apply(p) == '.' || apply(p) == 'S' }.toList
      val nOddTiles = tiles.count{p => (p dist start) % 2 == 1 }
      val nEvenTiles = tiles.size - nOddTiles
      // Everything within ceil(n / cols) should be reachable probably
      val radius = ((n - cols/2) / cols) - 1// number of maps we can traverse in one direction
      val remain = n - radius*cols - cols/2
      val reach = math.sumOver(remain) // number of tiles we could reach in remaining N steps
      // sum(1 to radius + 1) + sum(1 to radius - 1)
      val fullyReachable = 4*math.sumOver(radius) + 1
      val partialReachable = 4*(radius+1)

      // sum
      val totalEven = 4*math.sumOverEven(radius) + 1 // sum(r=2 to radius by 2)(4*radius) = 4*r/2*(r/2+1)
      val totalOdd = 4*math.sumOverOdd(radius)   // sum(r=1 to radius by 2)(4*radius) =
      val fullMaps = {
        if (stepsIsOdd) totalOdd*nEvenTiles + totalEven*nOddTiles
        else            totalOdd*nOddTiles + totalEven*nEvenTiles
      }
      val partialMaps = {
        val top = reachable(Pos(rows - 1, start.col), remain).size.toLong
        val bottom = reachable(Pos(0, start.col), remain).size.toLong
        val left = reachable(Pos(start.row, cols - 1), remain).size.toLong
        val right = reachable(Pos(start.row, 0), remain).size.toLong
        val top_left = radius * reachable(Pos(rows - 1, cols - 1), remain).size.toLong
        val top_right = radius * reachable(Pos(rows - 1, 0), remain).size.toLong
        val bottom_left = radius * reachable(Pos(0, cols - 1), remain).size.toLong
        val bottom_right = radius * reachable(Pos(0, 0), remain).size.toLong
        top + bottom + left + right + top_left + top_right + bottom_left + bottom_right
      }

      // val density = (tiles.toDouble / cols*rows)
      println(s"Partial: $partialMaps")
      println(s"Tiles: ${tiles.size}, rows:$rows, cols:$cols, radius:$radius, remain:$remain, reach:$reach, full:$fullyReachable")
      println(s"Even Tiles: $nEvenTiles, Odd Tiles: $nOddTiles (check: ${nEvenTiles + nOddTiles} == ${tiles.size})")
      println(s"Even repeats: $totalEven, Odd Repeats: $totalOdd (check: ${totalEven + totalOdd} == $fullyReachable)")
      println(s"Guess: ${fullMaps + partialMaps}")
      println(s"Refer: ${4*math.sumOver(n) + 1}")
      println(s"Prev0: 639133417909820 (too high)")
      println(s"Prev1: 639127089965820 (?)")
      println(s"Prev2: 639126042932833 (?)")
    }
  }



  val map = new GardenMap(data.getLines().map(_.toArray))
  // map.reachableAt(5000)
  map.reachableAt(26501365)

  /*(5 until 10).foreach { radius =>
    val matrix = common.immutable.Matrix[Char]((0 until radius * 2 + 1).iterator.map { i =>
      (0 until (radius*2 + 1)).iterator.map { j =>
        val dist = Math.abs(i - radius) + Math.abs(j - radius)
        if (dist < radius && dist % 2 == 1) 'O' else if (dist < radius && dist % 2 == 0) 'E' else '.'
      }.toArray
    })
    val even = matrix.posIterator().count { p => matrix(p) == 'E' }
    val odd = matrix.posIterator().count { p => matrix(p) == 'O' }
    println(matrix)
    println(s"radius:$radius, even:$even, odd:$odd")
  }*/
}
