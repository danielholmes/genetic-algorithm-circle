package org.danielholmes.gacirc

import scala.annotation.tailrec

case class SimulationState(
  crossoverRate: Double,
  mutationRate: Double,
  fitnessFunction: (Chromosome) => Double,
  randomiser: () => Double,
  population: Traversable[Chromosome]
) {
  require(crossoverRate >= 0 && crossoverRate <= 1)
  require(mutationRate >= 0 && mutationRate <= 1)

  lazy val results = GenerationResults(
    population.map(d => ChromosomeResult(d, fitnessFunction.apply(d)))
  )

  private lazy val nextPopulation = {
    if (population.size <= 1) {
      population
    } else {
      reproduce(Traversable.empty)
    }
  }

  @tailrec
  private def reproduce(current: Traversable[Chromosome]): Traversable[Chromosome] = {
    if (current.size == population.size) {
      current
    } else {
      reproduce(current ++ tryToReproduce())
    }
  }

  private def tryToReproduce(): Option[Chromosome] = {
    val resultsList = results.chromosomeResults.toList
    val parent1 = chooseParent(resultsList)
    val parent1Index = resultsList.map(_.chromosome).indexOf(parent1)
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
    // TODO:
    chromosome
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

  lazy val nextState = SimulationState(crossoverRate, mutationRate, fitnessFunction, randomiser, nextPopulation)
}
