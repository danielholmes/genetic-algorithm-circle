package org.danielholmes.gacirc

import scala.annotation.tailrec

class SimulationEnvironment(
  crossoverRate: Double,
  mutationRate: Double,
  fitnessFunction: (Chromosome) => Double,
  randomiser: () => Double
) {
  require(crossoverRate >= 0 && crossoverRate <= 1)
  require(mutationRate >= 0 && mutationRate <= 1)

  def runGeneration(state: SimulationState): GenerationResults = {
    val results = state.population.map(d => ChromosomeResult(d, fitnessFunction.apply(d)))
    GenerationResults(
      results,
      SimulationState(nextPopulation(state, results))
    )
  }

  private def nextPopulation(state: SimulationState, results: Traversable[ChromosomeResult]): Traversable[Chromosome] = {
    if (state.population.size <= 1) {
      state.population
    } else {
      reproduce(Traversable.empty, state.population.size, results)
    }
  }

  @tailrec
  private def reproduce(
    current: Traversable[Chromosome],
    targetSize: Int,
    results: Traversable[ChromosomeResult]
  ): Traversable[Chromosome] = {
    if (current.size == targetSize) {
      current
    } else {
      reproduce(current ++ tryToReproduce(results), targetSize, results)
    }
  }

  private def tryToReproduce(results: Traversable[ChromosomeResult]): Option[Chromosome] = {
    val resultsList = results.toList
    val parent1 = chooseParent(resultsList)
    val split = resultsList.span(_.chromosome != parent1)
    val newFrom = split._1 ::: split._2.tail
    assert(newFrom.size == resultsList.size - 1)
    val parent2 = chooseParent(newFrom)
    tryToReproduce(parent1, parent2)
  }

  private def tryToReproduce(parent1: Chromosome, parent2: Chromosome): Option[Chromosome] = {
    if (randomiser.apply() > crossoverRate) {
      None
    } else {
      Some(mutate(crossover(parent1, parent2)))
    }
  }

  private def crossover(parent1: Chromosome, parent2: Chromosome): Chromosome = {
    val positionRatio = randomiser.apply()
    val index1 = Math.floor(positionRatio * parent1.genes.size).toInt
    val index2 = Math.floor(positionRatio * parent2.genes.size).toInt
    Chromosome(parent1.genes.slice(0, index1) ++ parent2.genes.slice(index2, parent2.genes.size))
  }

  private def mutate(chromosome: Chromosome): Chromosome = {
    Chromosome(chromosome.genes.map(g => mutate(0, g)))
  }

  @tailrec
  private def mutate(index: Int, gene: Gene): Gene = {
    if (index >= gene.bitSize) {
      gene
    } else if (randomiser.apply() <= mutationRate) {
      mutate(index + 1, gene.mutate(index))
    } else {
      mutate(index + 1, gene)
    }
  }

  private def chooseParent(from: List[ChromosomeResult]): Chromosome = {
    val fitnessIndex = randomiser.apply() * from.map(_.fitness).sum
    chooseParent(from, fitnessIndex)
  }

  @tailrec
  private def chooseParent(from: List[ChromosomeResult], fitnessIndex: Double): Chromosome = {
    from match {
      case Nil => throw new RuntimeException("Invalid state")
      case x :: Nil => x.chromosome
      case x :: xs =>
        if (x.fitness >= fitnessIndex) {
          x.chromosome
        } else {
          chooseParent(xs, fitnessIndex - x.fitness)
        }
    }
  }
}
