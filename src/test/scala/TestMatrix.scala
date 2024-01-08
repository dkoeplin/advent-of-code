import common.immutable.{Matrix, Pos}
import common.parse
import org.scalatest.flatspec._
import org.scalatest.matchers._

class TestMatrix extends AnyFlatSpec with should.Matchers {
  "Matrix" should "index" in {
    val matrix = Matrix(Pos(32, 32), Array.fill(32*32)(0))
    matrix.indices.foreach{p => matrix.has(p) should be (true) }
    matrix.shape.strides should be (Pos(32, 1))
    matrix.indices.foreach{p => matrix(p) should be (0) }
  }

  "Matrix" should "iterate over row" in {
    val matrix = Matrix(Pos(32, 32), Array.fill(32*32)(0))
    val row = matrix.row(13)
    row.toList should be (List.fill(32)(0))
  }

  "Matrix" should "parse" in {
    val lines = Seq("ABAB", "BABA", "BAAB")
    val matrix = parse.chars(lines).to[Matrix]
    matrix.shape should be (Pos(3, 4))
    matrix.W should be (4)
    matrix.H should be (3)
  }
}
