package Year2022

import common.immutable.Pos.Idx
import common.mutable.SparseMatrix

object Day17 extends common.AoC(17, 2022) {
  case class Rock(pos: Idx, kind: Int) {
    def pixels(): Array[Idx] = Rock.getPixels(pos, kind)
    def move(delta: Idx, grid: SparseMatrix[Char]): Option[Rock] = {
      Some(Rock(pos + delta, kind)).filter(_.valid(grid))
    }
    def valid(grid: SparseMatrix[Char]): Boolean = pixels().forall{pos =>
      pos.c >= 0 && pos.c <= 6 && grid(pos) == '.'
    }
  }
  object Rock extends Enumeration {
    private def pixels(seq: String): Array[Idx] = {
      seq.split('/').zipWithIndex.flatMap{case (line, i) =>
        line.zipWithIndex.flatMap{case (c, j) =>
          Some(Idx(i, j)).filter(_ => c == '#')
        }
      }
    }
    private def getPixels(pos: Idx, kind: Int): Array[Idx] = {
      types(kind).map(_ + pos)
    }
    private lazy val types = Array(
      pixels("####"),
      pixels(".#./###/.#."),
      pixels("..#/..#/###"),
      pixels("#/#/#/#"),
      pixels("##/##")
    )
    private lazy val heights = Array(1, 3, 3, 4, 2)
    val kinds: Int = types.length
    def height(kind: Int): Int = heights(kind)
  }

  val jets = data.getLines().flatten.toArray

  def simulate(rock: Int, jet: Int, grid: SparseMatrix[Char]): Int = {
    var jetIndex = jet
    val kind = rock % Rock.kinds
    val minY = grid.min.r - 3 - Rock.height(kind)
    var prev: Option[Rock] = None
    var next: Option[Rock] = Some(Rock(Idx(minY, 2), kind))
    while (next.nonEmpty) {
      val move = jets(jetIndex) match {
        case '<' => Idx.D2.L
        case '>' => Idx.D2.R
      }
      prev = next.flatMap(_.move(move, grid)).orElse(next)
      jetIndex = (jetIndex + 1) % jets.length
      next = prev.orElse(next).flatMap(_.move(Idx.D2.D, grid))
    }
    prev.get.pixels().foreach{p => grid(p) = '#' }
    jetIndex
  }

  def part1(): Long = {
    val grid = new SparseMatrix[Char]('.')
    (0 to 6).foreach{j => grid(Idx(0, j)) = '-' }
    var jet = 0
    (0 until 2022).foreach {rock =>
      jet = simulate(rock % Rock.kinds, jet, grid)
    }
    -grid.min.r
  }
  println(s"Part1: ${part1()}")

  case class Pair(h: Long, r: Long)
  case class Pattern(init: Pair, steady: Pair, rem: Pair)
  def part2(max: BigInt): Pattern = {
    val grid = new SparseMatrix[Char]('.')
    (0 to 6).foreach { j => grid(Idx(0, j)) = '-' }
    def surface(): Seq[Long] = {
      val minY = grid.min.r
      (0 to 6).map{j =>
        grid.keys.filter(_.c == j).minBy(_.r).r - minY
      }
    }
    case class State(jet: Int, rock: Int, surface: Seq[Long])

    var rock: Int = 0
    var jet: Int = 0
    val states = scala.collection.mutable.Map.empty[State, Pair]
    var pattern: Option[Pattern] = None
    var rocks: Int = 0
    var maxRocks: Int = 0
    var prevHeight: Long = 0
    while (rocks < 1000000 && (pattern.isEmpty || rocks < maxRocks)) {
      if (pattern.isEmpty) {
        val state = State(jet, rock, surface())
        val height = -grid.min.r
        states.get(state) match {
          case None => states(state) = Pair(height, rocks)
          case Some(Pair(prevH, prevR)) =>
            val deltaH = height - prevH
            val deltaR = rocks - prevR
            val rem = ((max - rocks) % deltaR).toInt
            val p = Pattern(init=Pair(height, rocks), steady=Pair(deltaH, deltaR), rem=Pair(0, rem))
            pattern = Some(p)
            maxRocks = rocks + rem
            prevHeight = height
            states(state) = Pair(height, rocks)
        }
      }
      jet = simulate(rock, jet, grid)
      rock = (rock + 1) % Rock.kinds
      rocks += 1
    }
    val height = -grid.min.r
    val deltaH = height - prevHeight
    val Pattern(init, steady, Pair(_, rem)) = pattern.get
    Pattern(init, steady, Pair(deltaH, rem))
  }
  val total = BigInt("1000000000000")
  val pattern = part2(total)
  val n: BigInt = (total - pattern.init.r) / pattern.steady.r
  val h: BigInt = pattern.init.h + pattern.rem.h + (n * pattern.steady.h)
  println(s"Found pattern: ")
  println(s"  Init: h=${pattern.init.h}, r=${pattern.init.r}")
  println(s"  Stdy: h=${pattern.steady.h}, r=${pattern.steady.r}")
  println(s"  Rem:  h=${pattern.rem.h}, r=${pattern.rem.r}")
  println(s"Total Patterns: $n")
  println(s"Total Height: $h")
}

