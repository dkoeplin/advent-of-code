package Year2023

import common.immutable.Pos.Idx
import common.immutable.{Box, Constructable, Matrix}
import common.{math, parse}

object Day21 extends common.AoC(21, 2023) {
  case class GardenMap(vol: Box[Int], data: Array[Char]) extends Matrix(vol, data) {
    lazy val start: Idx = indices.find{p => apply(p) == 'S' }.get
    def next(p: Idx): Iterator[Idx]
      = Idx.D2.nondiag.iterator.map(_ + p).filter { p => get(p).exists { c => c == '.' || c == 'S' } }

    def reachable(begin: Idx, n: Int): Set[Idx] = (1 to n).foldLeft(Set(begin)){ (f, _) => f.flatMap(next) }

    def reachableAt(n: Int): Unit = {
      val stepsIsOdd = n%2 == 0
      val tiles = indices.filter{p => apply(p) == '.' || apply(p) == 'S' }.toList
      val nOddTiles = tiles.count{p => (p dist start) % 2 == 1 }
      val nEvenTiles = tiles.size - nOddTiles
      // Everything within ceil(n / cols) should be reachable probably
      val radius = ((n - W/2) / W) - 1// number of maps we can traverse in one direction
      val remain = n - radius*W - W/2
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
        val top = reachable(Idx(H - 1, start.c), remain).size.toLong
        val bottom = reachable(Idx(0, start.c), remain).size.toLong
        val left = reachable(Idx(start.r, W - 1), remain).size.toLong
        val right = reachable(Idx(start.r, 0), remain).size.toLong
        val top_left = radius * reachable(Idx(H - 1, W - 1), remain).size.toLong
        val top_right = radius * reachable(Idx(H - 1, 0), remain).size.toLong
        val bottom_left = radius * reachable(Idx(0, W - 1), remain).size.toLong
        val bottom_right = radius * reachable(Idx(0, 0), remain).size.toLong
        top + bottom + left + right + top_left + top_right + bottom_left + bottom_right
      }

      // val density = (tiles.toDouble / cols*H)
      println(s"Partial: $partialMaps")
      println(s"Tiles: ${tiles.size}, rows:$H, cols:$W, radius:$radius, remain:$remain, reach:$reach, full:$fullyReachable")
      println(s"Even Tiles: $nEvenTiles, Odd Tiles: $nOddTiles (check: ${nEvenTiles + nOddTiles} == ${tiles.size})")
      println(s"Even repeats: $totalEven, Odd Repeats: $totalOdd (check: ${totalEven + totalOdd} == $fullyReachable)")
      println(s"Guess: ${fullMaps + partialMaps}")
      println(s"Refer: ${4*math.sumOver(n) + 1}")
      println(s"Prev0: 639133417909820 (too high)")
      println(s"Prev1: 639127089965820 (?)")
      println(s"Prev2: 639126042932833 (?)")
    }
  }
  implicit object GardenMap extends Constructable[Char,GardenMap]



  val map = parse.chars(data).to[GardenMap]
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
