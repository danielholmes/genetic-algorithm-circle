package org.danielholmes.gacirc

import org.scalatest._

class SimulationStateSpec extends FlatSpec with Matchers {
  "SimulationState" should "run empty population correctly" in {
    val fitnessFunction: (Chromosome => Double) = (d) => 1
    val randomiser: () => Double = () => 0
    val s = SimulationState(0.5, 0, fitnessFunction, randomiser, Traversable.empty)
    s.results should be (GenerationResults(Traversable.empty))
    s.nextState should be (SimulationState(0.5, 0, fitnessFunction, randomiser, Traversable.empty))
  }

  it should "run singleton population correctly" in {
    val fitnessFunction: (Chromosome => Double) = (d) => 1
    val randomiser: () => Double = () => 0
    val c1 = Chromosome(Seq(IntGene(1)))
    val s = SimulationState(0.5, 0, fitnessFunction, randomiser, Traversable(c1))
    s.results should be (GenerationResults(Traversable(ChromosomeResult(c1, 1))))
    s.nextState should be (SimulationState(0.5, 0, fitnessFunction, randomiser, Traversable(c1)))
  }

  it should "run multiple population correctly" in {
    val fitnessFunction: (Chromosome => Double) = (d) => 2
    val randomiser: () => Double = () => 0.4
    val c1 = Chromosome(Seq(IntGene(1), IntGene(2), IntGene(3)))
    val c2 = Chromosome(Seq(IntGene(4), IntGene(5), IntGene(6)))
    val s = SimulationState(1, 0, fitnessFunction, randomiser, Traversable(c1, c2))
    s.results should be (GenerationResults(
      Traversable(ChromosomeResult(c1, 2), ChromosomeResult(c2, 2))
    ))
    s.nextState.population.size should be (2)
    // Our supplied randomiser and no mutation allows only 2 kinds of chromosomes
    // Got to be an easier way to do this assertion
    s.nextState.population.foreach({
      _ should (
        be (Chromosome(Seq(IntGene(1), IntGene(5), IntGene(6)))) or
          be (Chromosome(Seq(IntGene(4), IntGene(2), IntGene(3))))
        )
    })
  }
}
