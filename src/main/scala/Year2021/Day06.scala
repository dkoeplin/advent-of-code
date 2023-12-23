package Year2021

import scala.collection.mutable

object Day06 extends App {
  var fish = mutable.Seq.fill[Long](9)(0)
  val file = scala.io.Source.fromFile("./data/6")
  file.foreach { case c if c != ',' => fish(c - '0') += 1 case _ => }
  file.close()

  (0 until 256).foreach { i =>
    val next = fish(i % 7)
    fish(i % 7) += fish(7)
    fish(7) = fish(8)
    fish(8) = next
  }

  println(s"Part 2 (Fish): ${fish.sum}")
}
