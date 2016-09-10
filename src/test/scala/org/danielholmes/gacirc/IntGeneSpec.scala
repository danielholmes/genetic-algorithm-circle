package org.danielholmes.gacirc

import org.scalatest._

class IntGeneSpec extends FlatSpec with Matchers {
  "IntGene" should "correctly show bitSize" in {
    IntGene(1, 4).bitSize should be (4)
  }

  it should "throw error if bit size not big enough" in {
    an [IllegalArgumentException] should be thrownBy {
      IntGene(10000, 2)
    }
  }

  it should "return correct bits" in {
    IntGene(1, 4).toBits should be (Seq(false, false, false, true))
  }

  it should "mutate in range bit" in {
    IntGene(0, 4).mutate(3) should be (IntGene(1, 4))
  }

  it should "throw error if mutate out of range bit" in {
    an [IllegalArgumentException] should be thrownBy {
      IntGene(0, 4).mutate(4)
    }
  }
}
