package org.danielholmes.gacirc

class Fitness(surface: Surface, encoder: GeneEncoder) {
  def calculate(chromosome: Chromosome): Double = {
    val disk = encoder.decode(chromosome)
    if (!surface.withinBounds(disk) || surface.anyOverlaps(disk)) {
      0
    } else {
      disk.radius
    }
  }
}
