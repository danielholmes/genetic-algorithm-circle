package org.danielholmes.gacirc

import org.scalatest._

class SurfaceSpec extends FlatSpec with Matchers {
  "Surface" should "return properties" in {
    val s = Surface(400, 300)
    s.width should be (400)
    s.height should be (300)
  }

  it should "hold disks" in {
    val s = Surface(400, 300, Set(Disk(10, 5, 5)))
    s.disks should be (Set(Disk(10, 5, 5)))
  }

  it should "throw error if disk outside of area" in {
    an [IllegalArgumentException] should be thrownBy {
      Surface(400, 300, Set(Disk(4, 4, 5)))
    }
  }

  it should "throw error if disks overlap" in {
    an [IllegalArgumentException] should be thrownBy {
      Surface(400, 300, Set(Disk(5, 5, 5), Disk(6, 6, 5)))
    }
  }

  it should "find correctly if positive disk overlap" in {
    Surface(400, 300, Set(Disk(5, 5, 5), Disk(20, 20, 5))).anyOverlaps(Disk(19, 19, 5)) should be (true)
  }

  it should "find correctly if negative disk overlap" in {
    Surface(400, 300, Set(Disk(5, 5, 5), Disk(20, 20, 5))).anyOverlaps(Disk(100, 100, 5)) should be (false)
  }
}
