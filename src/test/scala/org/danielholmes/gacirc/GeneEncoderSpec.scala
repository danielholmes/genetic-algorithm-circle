package org.danielholmes.gacirc

import org.scalatest._

class GeneEncoderSpec extends FlatSpec with Matchers {
  val encoder = new GeneEncoder(6, 5, 4)

  "GeneEncoder" should "correctly decode chromosome" in {
    val c = Chromosome(HalfIntGene(1, 4), HalfIntGene(1.5, 4), IntGene(3, 4))
    encoder.decode(c) should be (Disk(1, 1.5, 3))
  }
}
