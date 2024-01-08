package Year2023

import scala.io.BufferedSource

object Day24 extends common.AoC(24, 2023) {
  case class Pos2D(x: Double, y: Double) {
    def dist(rhs: Pos2D): Double = Math.sqrt(Math.pow(x - rhs.x, 2) + Math.pow(y - rhs.y, 2))
    override def toString: String = s"(x = $x, y = $y)"
  }
  // y = m*x + b
  case class Line2D(m: Double, b: Double) {
    def intersect(rhs: Line2D): Option[Pos2D] = if (rhs.m == m) None else {
      // m0*x + b0 = m1*x + b1
      // x = (b1 - b0)/(m0 - m1)
      val x = (b - rhs.b)/(rhs.m - m)
      val y = m*x + b
      Some(Pos2D(x, y))
    }
  }
  case class Intersection(a: Hailstone, b: Hailstone, pos: Pos2D) {
    def dist: Double = Math.min(a.start2D dist pos, b.start2D dist pos)
  }
  // Describes a hailstone moving along the line
  // {x = px + vx*t, y = py + vy*t, z = pz + pz*t}
  case class Hailstone(px: Double, py: Double, pz: Double, vx: Double, vy: Double, vz: Double) {
    def start2D: Pos2D = Pos2D(px, py)
    // x = px + vx*t, t = (x - px)/vx
    // y = py + vy*t, y = py + (vy/vx)*x - px*vy/vx
    def m: Double = vy / vx
    def b: Double = py - px*m
    def trace: Line2D = Line2D(m, b)
    def intersect(b: Hailstone): Option[Intersection] = trace.intersect(b.trace).map{pos => Intersection(this, b, pos) }
    override def toString: String = s"$px, $py, $pz @ $vx, $vy, $vz"
  }
  object Hailstone {
    def parse(line: String): Hailstone = {
      val Array(p, v) = line.split("@")
      val Array(px, py, pz) = p.trim.split(", ").map(_.trim).map(_.toDouble)
      val Array(vx, vy, vz) = v.trim.split(", ").map(_.trim).map(_.toDouble)
      Hailstone(px, py, pz, vx, vy, vz)
    }
  }
  case class DoubleRange(min: Double, max: Double) {
    def contains(x: Double): Boolean = x >= min && x <= max
  }
  def intersections(hailstones: Array[Hailstone], xRange: DoubleRange, yRange: DoubleRange): List[Intersection] = {
    hailstones.combinations(2)
      .flatMap{case Array(a, b) => a intersect b }
      .toArray.sortBy(_.dist)
      .foldLeft((Set.empty[Hailstone], List.empty[Intersection])){case ((intersected, intersections), i) =>
        println(s"Hailstone A: ${i.a} (${i.a.start2D dist i.pos})")
        println(s"Hailstone B: ${i.b} (${i.b.start2D dist i.pos})")
        if (!xRange.contains(i.pos.x) || !yRange.contains(i.pos.y)) {
          println(s"Collision outside test range at ${i.pos}")
          (intersected ++ Seq(i.a, i.b), intersections)
        } else if (!intersected.contains(i.a) && !intersected.contains(i.b)) {
          println(s"Collision within test range at ${i.pos}")
          (intersected ++ Seq(i.a, i.b), i +: intersections)
        } else if (!intersected.contains(i.b)) {
          println(s"Hailstone A path already crossed")
          (intersected, intersections)
        } else if (!intersected.contains(i.a)) {
          println(s"Hailstone B path already crossed")
          (intersected, intersections)
        } else {
          println(s"Both hailstones' paths already crossed")
          (intersected, intersections)
        }
      }._2
  }
  def intersections(name: String, file: BufferedSource, range: DoubleRange): Unit = {
    val hailstones = file.getLines().map(Hailstone.parse).toArray
    val example = intersections(hailstones, range, range)
    println(s"Part 1 ($name): ${example.length}")
  }

  intersections("example", example(), DoubleRange(7, 27))
  // intersections("data", data, DoubleRange(200000000000000L, 400000000000000L))
}
