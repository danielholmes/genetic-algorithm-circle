package org.danielholmes.gacirc

import scala.swing._
import scala.util.Random

object App extends SimpleSwingApplication {
  def top = new MainFrame() {
    title = "Genetic Algorithm - largest non-overlapping disk"

    val setupPanel = new SetupPanel()
    listenTo(setupPanel)

    private def start(config: Config): Unit = {
      val initialPopulation = Disk.createRandomWithinBounds(
        config.populationSize,
        config.surface.width,
        config.surface.height
      )
      start(config, initialPopulation)
    }

    private def start(config: Config, initialPopulation: Traversable[Disk]): Unit = {
      val encoder = new GeneEncoder(
        HalfIntGene.smallestBitSizeFor(config.surface.width),
        HalfIntGene.smallestBitSizeFor(config.surface.height),
        IntGene.smallestBitSizeFor(Math.ceil(config.surface.height / 2))
      )

      val env = new SimulationEnvironment(
        config.crossoverRate,
        config.mutationRate,
        new Fitness(config.surface, encoder),
        new Random().nextDouble
      )
      val runPanel = new RunPanel(
        config.surface,
        env,
        SimulationState(encoder.encode(initialPopulation))
      )
      listenTo(runPanel)
      contents = runPanel
    }

    reactions += {
      case SetupDone(config) => start(config)
      case BackToSetupRequested() => contents = setupPanel
      case RestartRequested(i) => start(setupPanel.config, i)
    }

    contents = setupPanel
  }
}
