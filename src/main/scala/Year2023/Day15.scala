package Year2023

import scala.collection.immutable.VectorMap

object Day15 extends Year2023(15) {
  case class Step(label: String, length: Option[Int]) { val box: Int = hash(label) }

  def hash(x: String): Int = x.foldLeft(0){(v, c) => (v + c)*17 % 256 }
  def power(box: (VectorMap[String, Int], Int)): Int
    = box._1.iterator.zipWithIndex.map{case ((_, len), slot) => (box._2 + 1) * (slot + 1) * len }.sum

  val sequence: Array[String] = data.getLines().next().split(',')
  val steps: Iterator[Step] = sequence.iterator.map{step =>
    if (step.endsWith("-")) Step(step.dropRight(1), None)
    else Step(step.dropRight(2), Some(step.last.asDigit))
  }
  // This HASHMAP is actually an Array :D
  val boxes = steps.foldLeft(Array.fill(256)(VectorMap.empty[String,Int])){(boxes, step) =>
    val box = boxes(step.box)
    val next = step.length.map{len => box + (step.label -> len)}.getOrElse(box.removed(step.label))
    (boxes.iterator.take(step.box) ++ Iterator(next) ++ boxes.iterator.drop(step.box + 1)).toArray
  }
  println(s"Part 1: ${sequence.map(hash).sum}")
  println(s"Part 2: ${boxes.iterator.zipWithIndex.map(power).sum}")
}
