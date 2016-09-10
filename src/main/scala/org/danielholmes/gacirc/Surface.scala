package org.danielholmes.gacirc

import scala.annotation.tailrec

case class Surface(width: Int, height: Int, disks: Set[Disk]) {
  require(disks.forall(d => d.leftX >= 0 && d.rightX <= width && d.topY >= 0 && d.bottomY <= height))
  require(Surface.noOverlappingDisks(disks))

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
}
