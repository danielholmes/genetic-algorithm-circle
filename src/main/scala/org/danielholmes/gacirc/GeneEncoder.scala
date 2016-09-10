package org.danielholmes.gacirc

class GeneEncoder(xBitSize: Int, yBitSize: Int, radiusBitSize: Int) {
  def decode(chromosome: Chromosome): Disk = {
    Disk(chromosome.xGene.value, chromosome.yGene.value, chromosome.radiusGene.value)
  }

  def encode(disks: Traversable[Disk]): Traversable[Chromosome] = {
    disks.map(encode)
  }

  def encode(disk: Disk): Chromosome = {
    Chromosome(
      HalfIntGene(disk.centreX, xBitSize),
      HalfIntGene(disk.centreY, yBitSize),
      IntGene(disk.radius, radiusBitSize)
    )
  }
}
