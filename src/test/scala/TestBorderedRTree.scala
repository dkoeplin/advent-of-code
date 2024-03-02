import common.immutable.View
import common.implicits.TupleOps._
import common.mutable.BorderedRTree
import org.scalatest.flatspec._
import org.scalatest.matchers._

class TestBorderedRTree extends AnyFlatSpec with should.Matchers with LabeledBoxes {
  "BorderedRTree" should "update borders" in {
    val box = ((1, 1) to (10, 10)) named 1
    val tree = new BorderedRTree[Int,LabeledBox](rank=2)
    tree.view += View(box)
    tree.borders().toSet should be (box.box.borders().toSet)

    ((-5, -5) to (15, 15)).iterator.map{pos =>
      if (tree.hasValueAt(pos)) 'X' else if (tree.hasBorderAt(pos)) 'O' else '.'
    }.grouped(21).foreach{line => println(line.mkString) }

    val rm = (1, 2) to (4, 4)
    tree.view -= View(box)
    tree.view ++= (box.box diff rm).zipWithIndex.map{case (box, i) => View(box.named(i + 2)) }

    println("-"*21)
    ((-5, -5) to (15, 15)).iterator.map{pos =>
      if (tree.hasValueAt(pos)) 'X' else if (tree.hasBorderAt(pos)) 'O' else '.'
    }.grouped(21).foreach{line => println(line.mkString) }

  }
}
