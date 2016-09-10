package org.danielholmes.gacirc

import org.scalatest._

class FitnessSpec extends FlatSpec with Matchers {
  val fitness = new Fitness(Surface(200, 100, Set(Disk(10, 10, 5), Disk(30, 10, 5))), new GeneEncoder(10, 10, 10))

  private def chromosome(x: Double, y: Double, radius: Int): Chromosome = {
    Chromosome(HalfIntGene(x, 10), HalfIntGene(y, 10), IntGene(radius, 10))
  }

  "Fitness" should "return 0 for disk outside of surface" in {
    fitness.calculate(chromosome(1, 1, 5)) should be (0)
  }

  it should "return 0 for disk touching another" in {
    fitness.calculate(chromosome(11, 11, 5)) should be (0)
  }

  it should "return > 0 for disk not touching anything" in {
    fitness.calculate(chromosome(40, 10, 5)) should be > 0.0
  }

  it should "return larger fitness for larger disks not touching anything" in {
    val fiveResult = fitness.calculate(chromosome(50, 10, 5))
    val tenResult = fitness.calculate(chromosome(50, 10, 10))
    tenResult should be > 0.0
    tenResult should be >= (2 * fiveResult)
  }
}
