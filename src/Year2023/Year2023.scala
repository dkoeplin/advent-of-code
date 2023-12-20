package Year2023

import scala.io.BufferedSource
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

class Year2023(n: Int) extends App {
  private val day = n.toString
  private val pad = "0" * (2 - day.length)
  private def exampleName(n: Int): String = s"example/2023/$pad$day${if (n >= 0) "-" + n.toString else ""}"
  def example(n: Int = -1): BufferedSource = io.Source.fromFile(exampleName(n))
  lazy val data = io.Source.fromFile("data/2023/" + pad + day)

  def write(filename: String, str: String): Unit = {
    Files.write(Paths.get(filename), str.getBytes(StandardCharsets.UTF_8))
  }
}
