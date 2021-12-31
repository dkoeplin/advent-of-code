import common.mutable.Matrix

object Day20 extends App {
  val window = Seq((-1,-1),(-1,0),(-1,1),(0,-1),(0,0),(0,1),(1,-1),(1,0),(1,1))

  val file = io.Source.fromFile("./data/20")
  val lines = file.getLines()
  val table: Array[Int] = lines.next().map{case '#' => 1 case '.' => 0}.toArray
  val data = lines.filter(_.nonEmpty).map{line => line.map{case '#' => 1 case '.' => 0}}
  val input: Matrix[Int] = Matrix(data)

  def enhance(x: Matrix[Int], default: Int): Matrix[Int] = Matrix((-1 to x.rows).iterator.map{i =>
    (-1 to x.cols).map { j =>
      val w = window.map{case (di, dj) => x.getOrElse(i + di, j + dj, default) }.mkString("")
      table(Integer.parseInt(w, 2))
    }
  })

  def enhanceN(x: Matrix[Int], steps: Int): Matrix[Int] = {
    (0 until steps).foldLeft(x){case (mat, i) =>
      val default = if (table(0) == 1) i % 2 else 0
      enhance(mat, default)
    }
  }

  val part1 = enhanceN(input, 2)
  println(s"Part 1 (Total): ${part1.sum}")

  val part2 = enhanceN(input, 50)
  println(s"Part 2 (Total): ${part2.sum}")
}
