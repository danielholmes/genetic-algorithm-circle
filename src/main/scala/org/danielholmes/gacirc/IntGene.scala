package org.danielholmes.gacirc

import scala.annotation.tailrec

case class IntGene(value: Int, bitSize: Int) extends Gene {
  require(toShortestBinaryString.length <= bitSize)

  def mutate(bitIndex: Int): Gene = {
    require(bitIndex >= 0 && bitIndex < bitSize)
    IntGene.fromBits(toBits.updated(bitIndex, !toBits(bitIndex)))
  }

  lazy val toBits = Seq.fill(bitSize - toShortestBits.size)(false) ++ toShortestBits

  private lazy val toShortestBits = toShortestBinaryString.map(_ == '1')

  private lazy val toShortestBinaryString = value.toBinaryString
}

object IntGene {
  def fromBits(bits: Seq[Boolean]): IntGene = {
    val raw = bits.map(if (_) '1' else '0').mkString
    IntGene(Integer.parseInt(raw, 2), bits.size)
  }

  def maxValueForBitSize(bitSize: Int): IntGene = {
    IntGene(Math.pow(2, bitSize).toInt - 1, bitSize)
  }

  // Poor, todo: Find proper equation
  def smallestBitSizeFor(value: Double): Int = {
    smallestBitSizeFor(1, value)
  }

  @tailrec
  private def smallestBitSizeFor(current: Int, value: Double): Int = {
    if (Math.pow(2, current) > value) {
      current
    } else {
      smallestBitSizeFor(current + 1, value)
    }
  }
}