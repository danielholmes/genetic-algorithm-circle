package org.danielholmes.gacirc

import org.scalatest._

class SimulationEnvironmentSpec extends FlatSpec with Matchers {
  private def intGene(value: Int): IntGene = IntGene(value, 4)
  private def halfIntGene(value: Int): HalfIntGene = HalfIntGene(value, 4)

  "SimulationEnvironment" should "run empty population correctly" in {
    val s = SimulationState(Traversable.empty)
    new SimulationEnvironment(0.5, 0, new HardcodedFitness(1), () => 0).runGeneration(s) should be (
      GenerationResults(Traversable.empty, SimulationState(Traversable.empty))
    )
  }

  it should "run singleton population correctly" in {
    val c1 = Chromosome(halfIntGene(1), halfIntGene(2), intGene(3))
    val s = SimulationState(Traversable(c1))
    new SimulationEnvironment(0.5, 0, new HardcodedFitness(1), () => 0).runGeneration(s) should be (
      GenerationResults(Traversable(ChromosomeResult(c1, 1)), SimulationState(Traversable(c1)))
    )
  }

  it should "reproduce multiple population correctly" in {
    val c1 = Chromosome(halfIntGene(1), halfIntGene(2), intGene(3))
    val c2 = Chromosome(halfIntGene(4), halfIntGene(5), intGene(6))
    val s = SimulationState(Traversable(c1, c2))
    val next = new SimulationEnvironment(1, 0, new HardcodedFitness(2), () => 0.4).runGeneration(s)
    next.chromosomeResults should be (Traversable(ChromosomeResult(c1, 2), ChromosomeResult(c2, 2)))
    next.nextState.population.size should be (2)
    // Our supplied randomiser and no mutation allows only 2 kinds of chromosomes
    // Got to be an easier way to do this assertion
    next.nextState.population.foreach({
      _ should (
        be (Chromosome(halfIntGene(1), halfIntGene(5), intGene(6))) or
          be (Chromosome(halfIntGene(4), halfIntGene(2), intGene(3)))
        )
    })
  }

  it should "mutate correctly" in {
    val c = Chromosome(HalfIntGene.maxValueForBitSize(4), HalfIntGene.maxValueForBitSize(4), IntGene.maxValueForBitSize(4))
    val s = SimulationState(Traversable(c, c))
    val next = new SimulationEnvironment(1, 1, new HardcodedFitness(2), () => 0).runGeneration(s)
    next.chromosomeResults should be (Traversable(ChromosomeResult(c, 2), ChromosomeResult(c, 2)))
    next.nextState.population.size should be (2)
    // Our supplied randomiser and 100% mutation allows only 1 kind of mutated chromosome
    // Got to be an easier way to do this assertion
    next.nextState.population.foreach({
      _ should be (Chromosome(HalfIntGene(0, 4), HalfIntGene(0, 4), IntGene(0, 4)))
    })
  }
}

class HardcodedFitness(value: Double) extends Fitness(Surface(200, 100), new GeneEncoder(4, 4, 4)) {
  override def calculate(chromosome: Chromosome): Double = value
}
