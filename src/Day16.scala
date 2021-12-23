import scala.collection.mutable

case class Header(version: Long, tp: Long)

abstract class Packet(val header: Header) {
  val id: Int = { Packet.id += 1; Packet.id }
  final override def toString: String = s"%$id"
  final override def hashCode(): Int = id
  def readable: String
  def evaluate: Long
}
case class Literal(override val header: Header, literal: Long) extends Packet(header) {
  def readable: String = s"%$id = ${header.version}.Const($literal)"
  def evaluate: Long = literal
}
case class Operator(override val header: Header, packets: Seq[Packet], end: Int) extends Packet(header) {
  def readable: String = s"%$id = ${header.version}.${Packet.opNames(header.tp)}(${packets.mkString(", ")})"
  def evaluate: Long = {
    val elems = packets.map(_.evaluate)
    header.tp match {
      case 0 => elems.sum
      case 1 => elems.product
      case 2 => elems.min
      case 3 => elems.max
      case 5 => if (elems(0) > elems(1)) 1L else 0L
      case 6 => if (elems(0) < elems(1)) 1L else 0L
      case 7 => if (elems(0) == elems(1)) 1L else 0L
      case tp => sys.error(s"Invalid packet type: $tp")
    }
  }
}

case class PacketBuilder(
  header: Header,
  byPacketCount: Boolean, // 0 - total length of subpackets (15b), 1 - # subpackets (11)
  end: Long,
  packets: mutable.Buffer[Packet] = mutable.Buffer.empty[Packet]
) {
  def reify(pos: Int): Operator = Operator(header, packets, pos)
}

object Packet {
  type Bits = String
  var id: Int = -1

  val opNames: Map[Long,String] = Map[Long,String](
    0L -> "Sum", 1L -> "Product", 2L -> "Min", 3L -> "Max",
    5L -> "GreaterThan",  6L -> "LessThan", 7L -> "Equals"
  )

  val hex: Map[Char,Bits] = Map[Char,Bits](
    '0' -> "0000", '1' -> "0001", '2' -> "0010", '3' -> "0011",
    '4' -> "0100", '5' -> "0101", '6' -> "0110", '7' -> "0111",
    '8' -> "1000", '9' -> "1001", 'A' -> "1010", 'B' -> "1011",
    'C' -> "1100", 'D' -> "1101", 'E' -> "1110", 'F' -> "1111")

  def parse(x: Bits): Seq[Packet] = {
    var pos: Int = 0
    def parseBool(): Boolean = { val value = x.charAt(pos) == '1'; pos += 1; value }
    def rawBits(n: Int): Bits = { val value = x.substring(pos, pos + n); pos += n; value }
    def parseInt(n: Int): Long = BigInt(rawBits(n), 2).longValue()

    def done(builder: PacketBuilder): Boolean = {
      if (builder.byPacketCount) builder.packets.size >= builder.end else pos >= builder.end
    }

    val packets = mutable.Buffer.empty[Packet]
    val stack = mutable.Stack[PacketBuilder]()

    def finishPacket(packet: Packet): Unit = {
      packets += packet
      if (stack.nonEmpty)
        stack.top.packets += packet
    }

    do {
      // Header
      // (version: 3b)(type: 3b)

      // Type 4 - Literal
      // [Header: 6b]1(4b)1(4b) ... 0(4b) (0 padding to 4 bits)

      // Operators
      // [Header: 6b]0(length of subpackets in bits: 15b)(subpackets)
      // [Header: 6b]1(number of subpackets: 11b)(subpackets)

      if (stack.nonEmpty && done(stack.top)) {
        val current = stack.pop()
        finishPacket(current.reify(pos))
      } else {
        val header = Header(version = parseInt(3), tp = parseInt(3))
        if (header.tp == 4) {
          var continue = true
          val bits = new mutable.StringBuilder
          while (continue) { continue = parseBool(); bits ++= rawBits(4) }
          val packet = Literal(header, BigInt(bits.mkString, 2).longValue())
          finishPacket(packet)
        } else {
          val byPacketCount = parseBool()
          val end = if (byPacketCount) parseInt(11) else parseInt(15) + pos
          stack.push(PacketBuilder(header, byPacketCount, end))
        }
      }
    } while (stack.nonEmpty && pos < x.length)

    packets
  }
}

object Day16 extends App {
  val file = scala.io.Source.fromFile("./data/16")
  file.getLines().zipWithIndex.filterNot(_._1.startsWith("#")).foreach{case (line, idx) =>
    println(s"Line ${idx+1}: $line:")
    val binary = line.flatMap{c => Packet.hex(c) }.mkString
    val packets = Packet.parse(binary)
    packets.foreach{p => println(s"  ${p.readable}") }
    val part1 = packets.foldLeft(0L){case (accum, packet) => accum + packet.header.version }
    val part2 = packets.last.evaluate
    println(s"  Part 1 (Version Sum): $part1")
    println(s"  Part 2 (Evaluation): $part2")
  }
}
