package org.danielholmes.gacirc

case class HalfIntGene(value: Double, bitSize: Int) extends Gene {
  require(value % 0.5 == 0.0)
  // Hack way to require, invokes IntGene checks
  require(doubleValueIntGene != null)

  def mutate(bitIndex: Int): Gene = {
    require(bitIndex >= 0 && bitIndex < bitSize)
    HalfIntGene.fromBits(toBits.updated(bitIndex, !toBits(bitIndex)))
  }

  private lazy val doubleValueIntGene = IntGene((value * 2).toInt, bitSize)

  lazy val toBits = doubleValueIntGene.toBits
}

object HalfIntGene {
  def fromBits(bits: Seq[Boolean]): HalfIntGene = {
    val int = IntGene.fromBits(bits)
    HalfIntGene(int.value / 2.0, bits.size)
  }
}