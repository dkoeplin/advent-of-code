package Year2021


object Target {
  private val Number = "\\-?[0-9]+"
  private val TargetPattern = s"target area: x=($Number)\\.\\.($Number), y=($Number)\\.\\.($Number)".r
  def unapply(x: String): Option[(Int,Int,Int,Int)] = x match {
    case TargetPattern(x0,x1,y0,y1) => Some((x0.toInt,x1.toInt,y0.toInt,y1.toInt))
    case _ => None
  }
}

object Day17 extends App {
  val file = scala.io.Source.fromFile("./data/17")
  //   T
  // V/ \
  // 0   |
  //     F
  file.getLines().foreach{case line @ Target(xMin, xMax, yMin, yMax) =>
    // xMin <= sum(0 .. Vx) <= xMax

    // Max height is sum(0 .. initial Y velocity) = Y*(Y+1)/2
    // Fall height is sum(0 .. falling steps) = N*(N+1)/2
    // Final Y = Max Height - Fall Height = sum(0 to Y) - sum(0 to N) = -sum(Y+1 to N)
    // > maximum height achievable is where initial Y velocity = yFinal - 1
    // > want # steps for Y direction is 2*Y + 2
    // > choose initial velocities such that
    //   yMin <= Y <= yMax - 1
    //   xMin <= sum(Max(0, X - 2*Y - 2) to X) <= xMax
    //
    // try
    //   xMin <= sum(Max(0, X - 2*yMax - 2) to X) <= xMax
    //   xMin <= X(X+1)/2 - (X - 2*yMax)(2*X - (4*yMax + 3)) <= xMax
    // 0.5*X^2 + 0.5*X - xMax = 0
    // sqrt(0.25 + 2*xMax) - 0.5
    // OR
    // 0.5*X^2 + 0.5*X - 2*X^2 + (8*yMax + 3)*X - xMax - 2*yMax*(4*yMax + 3) = 0
    // -1.5X^2 + (8*yMax + 3.5)*X - (xMax + 2*yMax*(4*yMax + 3)) = 0
    // ((8*yMax + 3.5) - sqrt((8*yMax + 3.5)^2 - 6*(xMax + 2*yMax*(4*yMax + 3)))/3

    val yAbs = Math.max(Math.abs(yMax), Math.abs(yMin))

    val y = yAbs - 1
    val xDouble = Math.sqrt(0.25 + 2.0*xMax) - 0.5
    var x = xDouble.toInt
    val ySteps = 2*y + 2
    val xSteps = Math.min(x, ySteps)
    var xEnd = x - xSteps
    var xFinal = x*(x + 1)/2 - xEnd*(xEnd + 1)/2

    if (x > ySteps) { // Reaches Y much quicker than X - initial X velocity needs to be higher
      x = (xMax / ySteps) + (ySteps / 2)
      xEnd = x - ySteps
      xFinal = x*(x + 1)/2 - xEnd*(xEnd + 1)/2
    }

    val maxY = y*(y + 1)/2

    println(s"$line:")
    println(s"  Part 1 (Initial Velocities): X: $x, Y: $y")
    println(s"    Final X: $xFinal, Final Y: ${-y - 1}; Y steps: $ySteps")
    println(s"    Maximum height: $maxY")
    println(s"    Year2021.Target: X: [$xMin,$xMax], Y: [$yMin,$yMax]")

    // Part 2
    // Doing this the bruteforce way cause lol
    def reaches(vyInit: Int, vxInit: Int): Boolean = {
      var failed = false
      var passed = false
      var vx: Int = vxInit
      var vy: Int = vyInit
      var x: Int = 0
      var y: Int = 0
      while (!failed && !passed) {
        x += vx
        y += vy
        vx = if (vx < 0) Math.min(vx + 1, 0) else Math.max(0, vx - 1)
        vy = vy - 1
        passed = (x >= xMin && x <= xMax) && (y >= yMin && y <= yMax)
        failed = x > xMax || y < yMin || (x < xMin && vx == 0)
      }
      passed && !failed
    }

    val reached = (-yAbs until yAbs).flatMap{vy =>
      (0 to xMax).map{vx => (vy,vx)}.filter{case (vy,vx) => reaches(vy, vx) }
    }
    val part2 = reached.length
    println(s"  Part 2 (Number of initial velocities): $part2")
  }
}
