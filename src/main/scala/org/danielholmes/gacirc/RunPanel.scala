package org.danielholmes.gacirc

import java.awt.Color

import scala.annotation.tailrec
import scala.swing._
import scala.swing.event._

class RunPanel(surface: Surface, env: SimulationEnvironment, initialState: SimulationState) extends BoxPanel(Orientation.Vertical) {
  private var currentResults: Option[GenerationResults] = None
  private var generationNumber = 0

  private def runGenerations(amount: Int): Unit = {
    runGenerations(1, amount)
    surfaceDisplay.repaint()
  }

  @tailrec
  private def runGenerations(current: Int, amount: Int): Unit = {
    if (current <= amount) {
      generationNumber += 1
      currentResults = Some(env.runGeneration(currentResults.map(_.nextState).getOrElse(initialState)))
      runGenerations(current + 1, amount)
    }
  }

  object oneButton extends Button {
    text = "1 Generation"
  }
  listenTo(oneButton)

  object tenButton extends Button {
    text = "10 Generations"
  }
  listenTo(tenButton)

  object oneHundredButton extends Button {
    text = "100 Generations"
  }
  listenTo(oneHundredButton)

  object backButton extends Button {
    text = "Back to Setup"
  }
  listenTo(backButton)

  object restartButton extends Button {
    text = "Restart"
  }
  listenTo(restartButton)

  val statusText = new Label

  val surfaceDisplay = new Panel {
    preferredSize = new Dimension(surface.width, surface.height)

    override def paint(g: Graphics2D): Unit = {
      super.paint(g)

      paintSurfaceBackground(g)
      paintResults(g)
      paintSurfaceDisks(surface.disks.toList, g)
      paintStatus(g)
    }

    private def paintStatus(g: Graphics2D): Unit = {
      statusText.text = generationNumber + ") " + currentResults.map(r => {
        r.fittestResult.chromosome.xGene.value + "x" + r.fittestResult.chromosome.yGene.value + " => " +
          r.fittestResult.chromosome.radiusGene.value + " F: " + r.fittestResult.fitness
      }).getOrElse("")
    }

    private def paintSurfaceBackground(g: Graphics2D): Unit = {
      g.setColor(Color.WHITE)
      g.fillRect(0, 0, surface.width, surface.height)
      g.setColor(Color.GRAY)
      g.drawRect(0, 0, surface.width, surface.height)
    }

    private def paintResults(g: Graphics2D): Unit = {
      currentResults.map(_.chromosomeResults).foreach(r => paintChromosomeResults(r.toList, g))
    }

    @tailrec
    private def paintChromosomeResults(disks: List[ChromosomeResult], g: Graphics2D): Unit = {
      disks match {
        case Nil =>
        case x :: xs =>
          paintChromosomeResult(x, g)
          paintChromosomeResults(xs, g)
      }
    }

    private def getResultColor(result: ChromosomeResult): Color = {
      val MIN_ALPHA = 30
      if (result.fitness == 0.0) {
        new Color(0, 0, 255, MIN_ALPHA)
      } else {
        // TODO: Better reference for fitness max
        val MAX_ALPHA = 230
        val MAX_FITNESS = Math.min(surface.width, surface.height) / 2
        new Color(
          255, 0, 0,
          Math.round(
            MIN_ALPHA + ((result.fitness / MAX_FITNESS) * (MAX_ALPHA - MIN_ALPHA))
          ).toInt
        )
      }
    }

    private def paintChromosomeResult(result: ChromosomeResult, g: Graphics2D): Unit = {
      g.setColor(getResultColor(result))
      g.drawOval(
        (result.chromosome.xGene.value - result.chromosome.radiusGene.value).toInt,
        (result.chromosome.yGene.value - result.chromosome.radiusGene.value).toInt,
        result.chromosome.radiusGene.value * 2,
        result.chromosome.radiusGene.value * 2
      )
    }

    @tailrec
    private def paintSurfaceDisks(disks: List[Disk], g: Graphics2D): Unit = {
      g.setColor(Color.BLACK)
      disks match {
        case Nil =>
        case x :: xs => {
          paintSurfaceDisk(x, g)
          paintSurfaceDisks(xs, g)
        }
      }
    }

    private def paintSurfaceDisk(disk: Disk, g: Graphics2D): Unit = {
      g.drawOval(disk.leftX.toInt, disk.topY.toInt, disk.diameter, disk.diameter)
    }
  }

  reactions += {
    case ButtonClicked(`oneButton`) => runGenerations(1)
    case ButtonClicked(`tenButton`) => runGenerations(10)
    case ButtonClicked(`oneHundredButton`) => runGenerations(100)
    case ButtonClicked(`backButton`) => publish(BackToSetupRequested())
    case ButtonClicked(`restartButton`) =>
      currentResults = None
      generationNumber = 0
      surfaceDisplay.repaint()
  }

  val toolbar = new FlowPanel {
    contents += oneButton
    contents += tenButton
    contents += oneHundredButton
    contents += restartButton
    contents += backButton
  }

  contents += toolbar
  contents += statusText
  contents += surfaceDisplay
}
