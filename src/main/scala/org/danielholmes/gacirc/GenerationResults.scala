package org.danielholmes.gacirc

case class GenerationResults(chromosomeResults: Traversable[ChromosomeResult], nextState: SimulationState) {
  lazy val fittestResult = chromosomeResults.maxBy(_.fitness)
}
