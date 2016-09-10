package org.danielholmes.gacirc

case class HalfIntGene(value: Double) extends Gene {
  require(value % 0.5 == 0.0)
}
