package Year2022

import java.awt.Robot
import java.awt.event.InputEvent
import scala.util.Random

object Testing extends App {
  val robot = new Robot()
  def click(): Unit = {
    robot.mousePress(InputEvent.BUTTON1_DOWN_MASK)
    val delay = Random.nextInt(100) + 50
    robot.delay(delay)
    robot.mouseRelease(InputEvent.BUTTON1_DOWN_MASK)
  }
  for (i <- 0 until 200) {
    click()
    val delay = Random.nextInt(100) + 100
    robot.delay(delay)
    click()
    val delay2 = Random.nextInt(300) + 2200
    robot.delay(delay2)
  }
}
