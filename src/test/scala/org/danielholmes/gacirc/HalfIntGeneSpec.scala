package org.danielholmes.gacirc

import org.scalatest._

class HalfIntGeneSpec extends FlatSpec with Matchers {
  "HalfIntGene" should "correctly show bitSize" in {
    HalfIntGene(1, 4).bitSize should be (4)
  }

  it should "throw error if bit size not big enough" in {
    // 2 ^ 2 gives 4 states - enough for 0, 0.5, 1.0, 1.5
    an [IllegalArgumentException] should be thrownBy {
      HalfIntGene(2, 2)
    }
  }

  it should "not throw error if bit size big enough and return correct bits" in {
    // 2 ^ 2 gives 4 states - enough for 0, 0.5, 1.0, 1.5
    HalfIntGene(1.5, 2).toBits should be (Seq(true, true))
  }

  it should "return correct bits" in {
    HalfIntGene(0, 4).toBits should be (Seq(false, false, false, false))
  }

  it should "return correct bits 2" in {
    HalfIntGene(0.5, 4).toBits should be (Seq(false, false, false, true))
  }

  it should "mutate in range bit" in {
    HalfIntGene(0, 4).mutate(3) should be (HalfIntGene(0.5, 4))
  }

  it should "mutate in range bit 2" in {
    HalfIntGene(0.5, 4).mutate(3) should be (HalfIntGene(0, 4))
  }

  it should "throw error if mutate out of range bit" in {
    an [IllegalArgumentException] should be thrownBy {
      HalfIntGene(0, 4).mutate(4)
    }
  }
}
