package org.danielholmes.gacirc

class Fitness(surface: Surface, encoder: GeneEncoder) {
  def calculate(chromosome: Chromosome): Double = {
    calculate(encoder.decode(chromosome))
  }

  private def calculate(disk: Disk): Double = {
    if (!surface.withinBounds(disk) || surface.anyOverlaps(disk)) {
      0
    } else {
      calculateValidDisk(disk)
    }
  }

  private def calculateValidDisk(disk: Disk): Double = Math.pow(disk.radius, 2)

  lazy val max: Double = calculateValidDisk(
    Disk(
      surface.width / 2,
      surface.height / 2,
      Math.ceil(Math.min(surface.width, surface.height) / 2.0).toInt
    )
  )
}
