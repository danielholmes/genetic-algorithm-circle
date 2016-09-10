package org.danielholmes.gacirc

trait Gene {
  val bitSize: Int
  val toBits: Seq[Boolean]
  def mutate(bitIndex: Int): Gene
}
