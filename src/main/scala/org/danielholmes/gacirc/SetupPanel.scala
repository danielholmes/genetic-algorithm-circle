package org.danielholmes.gacirc

import swing._
import event._

class SetupPanel extends BoxPanel(Orientation.Vertical) {
  object startButton extends Button {
    text = "Start"
  }
  listenTo(startButton)

  val mutationRateField = new TextField {
    columns = 5
    text = 0.02.toString
  }
  val crossoverRateField = new TextField {
    columns = 5
    text = 0.7.toString
  }
  val populationSizeField = new TextField {
    columns = 5
    text = 200.toString
  }

  def config: Config = {
    val mutationRate = mutationRateField.text.toDouble
    val crossoverRate = crossoverRateField.text.toDouble
    val populationSize = populationSizeField.text.toInt
    val surface = Surface.createRandom(600, 400, 20)
    Config(surface, crossoverRate, mutationRate, populationSize)
  }

  reactions += {
    case ButtonClicked(`startButton`) => publish(SetupDone(config))
  }

  contents += new Label("Mutation Rate:")
  contents += mutationRateField

  contents += new Label("Crossover Rate:")
  contents += crossoverRateField

  contents += new Label("Population Size:")
  contents += populationSizeField

  contents += startButton
  border = Swing.EmptyBorder(20)
}
