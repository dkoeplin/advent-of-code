import common.immutable.Tensor.Tuple3Ops
import common.immutable.{Pos, Tensor, TensorView}
import org.scalatest.flatspec._
import org.scalatest.matchers._

class TestTensor extends AnyFlatSpec with should.Matchers {
  "Tensor" should "add" in {
    val a: TensorView[Int] = (0 to 2, 0 to 2, 0 to 2){(x,y,z) => x + y + z }
    val b: TensorView[Int] = (0 to 2, 0 to 2, 0 to 2){(x,y,z) => x * y * z }
    val c = (a + b).to[Tensor]
    c.indices.foreach{case p@Pos(x, y, z) => c(p) should be ((x + y + z) + (x * y * z)) }
  }
}
