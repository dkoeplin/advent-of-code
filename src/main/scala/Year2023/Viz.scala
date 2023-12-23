package Year2023

import scala.swing._

object Viz extends App {
  class UI extends MainFrame {
    title = "NYSE"
    preferredSize = new Dimension(320, 240)
    contents = new Label("Here is the contents!")
    visible = true
  }
  val ui = new UI

}
