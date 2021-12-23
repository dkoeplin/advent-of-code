object Day08 extends App {
  val nums = Seq("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg")
  val reverseDictionary: Map[String,Int] = nums.zipWithIndex.map{case (word,v) => word -> v }.toMap

  def signature(c: Char, words: Seq[String]): List[Int] = words.groupBy(_.length).map{case (len, ws) => len -> ws.map{_.count(_ == c) }.sum }.toList.sortBy(_._1).map(_._2)

  val signatureMap: Map[List[Int],Char] = ('a' to 'g').map{c => signature(c, nums) -> c }.toMap

  val file = scala.io.Source.fromFile("./data/8")
  val count = file.getLines().foldLeft((0,0)){(accum, line) =>
    val input = line.split(" \\| ")
    val unique = input(0).split(" ")
    val output = input(1).split(" ")
    val mapping: Map[Char,Char] = ('a' to 'g').map{c => c -> signatureMap(signature(c, unique)) }.toMap

    val decoded = output.map{encoded => reverseDictionary(encoded.map{c => mapping(c)}.sorted) }
    val count = accum._1 + decoded.count{v => v == 1 || v == 4 || v == 8 || v == 7}
    val sum = accum._2 + decoded.mkString("").toInt
    (count, sum)
  }

  println(s"Part 1: (1, 4, 7, 8): ${count._1}")
  println(s"Part 2: (Sum): ${count._2}")
}
