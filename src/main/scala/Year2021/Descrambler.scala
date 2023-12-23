package Year2021

import scala.collection.{Map, View}

object Descrambler extends App {
  val letters = "TEAWERHFROOPABLET".map(_.toLower)
  val masterCounts = countLetters(letters)

  val banned = Set("bowat", "fetwa", "pheer", "blewart", "rebop", "blate", "frate", "trefa", "ablet", "bepat", "ferer", "frere")

  val file = scala.io.Source.fromFile("./data/words2.txt")
  val words: Map[Int, Seq[String]] = file.getLines().map(_.toLowerCase)
    .filter { w => w.length == 5 || w.length == 7 }
    .filter { w => valid(w, masterCounts).nonEmpty }
    .filter { w => !banned.contains(w) }
    .toSeq.groupBy(_.length)

  println(s"Filtered words. Remaining: ${words.size}")

  def countLetters(x: String): Map[Char, Int] = x.groupBy(x => x).view.mapValues(_.length).toMap

  def valid(word: String, remain: Map[Char, Int]): Option[Map[Char, Int]] = {
    val count = countLetters(word)
    val valid = count.keys.forall { c => remain.getOrElse(c, 0) >= count(c) }
    Some(count).filter(_ => valid)
  }

  def diff(a: Map[Char, Int], b: Map[Char, Int]): Map[Char, Int] = {
    a.map { case (c, n) => (c, n - b.getOrElse(c, 0)) }.toMap
  }

  case class Found(x: Seq[String], remain: Map[Char, Int]) {
    override def toString: String = x.mkString(" ")
  }

  def findRest(length: Int, remain: Map[Char, Int]): Iterator[(Map[Char, Int], String)]
  = words(length).iterator.flatMap { word => valid(word, remain).map { x => (x, word) } }

  def find(lengths: Seq[Int]): Seq[Found] = {
    var found: Seq[Found] = Seq(Found(Nil, countLetters(letters)))
    lengths.foreach{len =>
      found = found.groupBy(_.remain).iterator.flatMap{case (remain, fs) =>
        findRest(len, remain).flatMap { case (counts, word) =>
          val left = diff(remain, counts)
          fs.map { f => Found(f.x :+ word, left) }
        }
      }.toSeq
      println(s"Found ${found.length} words for length $len")
    }
    found
  }

  find(Seq(7, 5, 5)).sortBy(_.x.head).foreach { f => println(f) }
}
