package common

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.io.BufferedSource

class AoC(day: Int, year: Int) extends App {
  private val dayStr = day.toString
  private val pad = "0" * (2 - dayStr.length)
  private def exampleName(n: Int): String = s"example/$year/$pad$dayStr${if (n >= 0) s"-$n" else ""}"
  def example(n: Int = -1): BufferedSource = io.Source.fromFile(exampleName(n))

  lazy val data: BufferedSource = {
    val filename = s"data/$year/$pad$day"
    val filename2 = s"data/$year/$day"
    if (java.nio.file.Files.exists(java.nio.file.Paths.get(filename))) {
      io.Source.fromFile(filename)
    } else if (java.nio.file.Files.exists(java.nio.file.Paths.get(filename2))) {
      io.Source.fromFile(filename2)
    } else {
      assert(assertion=false, s"Cannot find data $filename or $filename2")
      null
    }
  }

  def write(filename: String, str: String): Unit = {
    Files.write(Paths.get(filename), str.getBytes(StandardCharsets.UTF_8))
  }
}
