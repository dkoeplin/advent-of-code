package common

import java.net.URI
import java.util.Scanner
import scala.util.matching.Regex

case class Page(url: String) {
  val contents: String = {
    val sc = new Scanner(new URI(url).toURL.openStream)
    val sb = new StringBuffer
    while (sc.hasNext) sb.append(sc.next)
    sb.toString
  }
}

case class Records(times: Array[Double]) {
  lazy val min: Double = times.min
  lazy val max: Double = times.max
  lazy val avg: Double = times.sum / times.length
  lazy val stddev: Double = Math.sqrt(times.iterator.map{t => Math.pow(avg - t, 2) }.sum/times.length)
}
object Records {
  private val regex = "([0-9]{2}):([0-9]{2}):([0-9]{2})".r
  private def parse(m: Regex.Match): Double = m.group(1).toDouble * 60 + m.group(2).toDouble + m.group(3).toDouble/60
  def parse(str: String): Records = {
    val data = regex.findAllMatchIn(str).map(parse).toArray
    Records(data.take(data.length / 2))
  }
}

object Leaderboard extends App {
  val site = "https://adventofcode.com/2023/leaderboard/day"
  (1 to 25).iterator.foreach{day =>
    val page = Page(s"$site/$day").contents
    val records = Records.parse(s"$page/$day")
    println(s"$day, ${records.min}, ${records.avg}, ${records.max}")
  }
}
