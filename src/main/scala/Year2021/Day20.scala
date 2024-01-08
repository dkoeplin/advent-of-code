package Year2021

import common.immutable.Pos.Idx
import common.immutable.{Matrix, Volume}

object Day20 extends common.AoC(20, 2021) {
  val window = Volume(Idx(-1,-1), Idx(1,1))

  val lines = data.getLines()
  val table = lines.next().map{case '#' => 1 case '.' => 0}.toArray
  val input = Matrix(lines.filter(_.nonEmpty).map{line => line.map{case '#' => 1 case '.' => 0 }})

  def enhance(x: Matrix[Int], default: Int): Matrix[Int] = {
    val volume = Volume(Idx(-1,-1), Idx(x.H,x.W))
    val data = volume.iterator.map{p =>
      table(Integer.parseInt(window.iterator.map(_ + p).map{p => x.getOrElse(p, default) }.mkString, 2))
    }
    Matrix(volume, data.toArray)
  }

  def enhanceN(x: Matrix[Int], steps: Int): Matrix[Int] = {
    (0 until steps).foldLeft(x) { case (mat, i) =>
      val default = if (table(0) == 1) i % 2 else 0
      enhance(mat, default)
    }
  }

  val part1 = enhanceN(input, 2)
  println(s"Part 1 (Total): ${part1.sum}")

  val part2 = enhanceN(input, 50)
  println(s"Part 2 (Total): ${part2.sum}")
}
