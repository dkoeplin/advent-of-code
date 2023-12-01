package Year2023

import scala.io.BufferedSource

class Year2023(n: Int) extends App {
  private val day = n.toString
  private val pad = ("0" * (2 - day.length))

  def example(n: Int = -1): BufferedSource = {
    val name = pad + day + (if (n >= 0) "-" + n.toString else "")
    io.Source.fromFile("example/2023/" + name)
  }
  lazy val data = io.Source.fromFile("data/2023/" + pad + day)
}
