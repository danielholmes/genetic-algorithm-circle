package org.danielholmes.gacirc

import scala.annotation.tailrec

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

  def maxValueForBitSize(bitSize: Int): HalfIntGene = {
    val int = IntGene.maxValueForBitSize(bitSize)
    HalfIntGene(int.value / 2.0, bitSize)
  }

  // Poor, todo: Find proper equation
  def smallestBitSizeFor(value: Double): Int = {
    smallestBitSizeFor(1, value)
  }

  @tailrec
  private def smallestBitSizeFor(current: Int, value: Double): Int = {
    if (Math.pow(2, current) / 2.0 > value) {
      current
    } else {
      smallestBitSizeFor(current + 1, value)
    }
  }
}