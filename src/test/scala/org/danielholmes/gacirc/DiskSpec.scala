package org.danielholmes.gacirc

import org.scalatest._

class DiskSpec extends FlatSpec with Matchers {
  "Disk" should "correctly calculate non-overlap" in {
    Disk(0, 0, 10).overlaps(Disk(20, 20, 1)) should be (false)
  }

  it should "correctly calculate overlap" in {
    Disk(0, 0, 10).overlaps(Disk(5, 5, 10)) should be (true)
  }

  it should "correctly calculate touching" in {
    Disk(0, 0, 5).overlaps(Disk(10, 0, 5)) should be (false)
  }
}
