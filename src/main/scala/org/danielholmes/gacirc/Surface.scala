package org.danielholmes.gacirc

import scala.annotation.tailrec

case class Surface(width: Int, height: Int, disks: Set[Disk]) {
  require(disks.forall(withinBounds))
  require(Surface.noOverlappingDisks(disks))

  def withinBounds(disk: Disk): Boolean = {
    disk.leftX >= 0 && disk.rightX <= width && disk.topY >= 0 && disk.bottomY <= height
  }

  def anyOverlaps(disk: Disk): Boolean = {
    anyOverlaps(disk, disks.toList)
  }

  @tailrec
  private def anyOverlaps(disk: Disk, against: List[Disk]): Boolean = {
    against match {
      case Nil => false
      case x :: xs => disk.overlaps(x) || anyOverlaps(disk, xs)
    }
  }
}

object Surface {
  def apply(width: Int, height: Int): Surface = {
    Surface(width, height, Set.empty)
  }

  private def noOverlappingDisks(disks: Set[Disk]): Boolean = {
    val diskList = disks.toList
    noOverlappingDisks(diskList, diskList)
  }

  @tailrec
  private def noOverlappingDisks(toCheck: List[Disk], against: List[Disk]): Boolean = {
    if (toCheck.isEmpty) {
      true
    } else {
      noOverlappingDisks(toCheck.head, against) && noOverlappingDisks(toCheck.tail, against)
    }
  }

  @tailrec
  private def noOverlappingDisks(toCheck: Disk, against: List[Disk]): Boolean = {
    against match {
      case Nil => true
      case x :: xs =>
        (toCheck == x || !toCheck.overlaps(x)) && noOverlappingDisks(toCheck, xs)
    }
  }

  def createRandom(width: Int, height: Int, numDisks: Int): Surface = {
    createRandomDisks(Surface(width, height), numDisks)
  }

  @tailrec
  private def createRandomDisks(current: Surface, num: Int): Surface = {
    if (current.disks.size == num) {
      current
    } else {
      createRandomDisks(createRandomDisk(current), num)
    }
  }

  private def createRandomDisk(current: Surface): Surface = {
    val possible = Disk.createRandomWithinBounds(current.width, current.height)
    if (current.anyOverlaps(possible)) {
      createRandomDisk(current)
    } else {
      Surface(current.width, current.height, current.disks + possible)
    }
  }
}
