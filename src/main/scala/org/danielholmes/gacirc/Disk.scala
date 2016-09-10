package org.danielholmes.gacirc

import scala.annotation.tailrec
import scala.util.Random

case class Disk(centreX: Double, centreY: Double, radius: Int) {
  require(centreX >= 0)
  require(centreY >= 0)
  require(radius >= 0) // TODO: Prob shouldnt allow 0 and catch this "higher up"

  lazy val topY = centreY - radius
  lazy val bottomY = centreY + radius
  lazy val leftX = centreX - radius
  lazy val rightX = centreX + radius

  lazy val diameter = radius * 2

  def overlaps(other: Disk): Boolean = {
    centreToCentreDistance(other) < (radius + other.radius)
  }

  private def centreToCentreDistance(other: Disk): Double = {
    Math.sqrt(Math.pow(centreX - other.centreX, 2) + Math.pow(centreY - other.centreY, 2))
  }
}

object Disk {
  private val random = new Random()

  def createRandomWithinBounds(amount: Int, width: Int, height: Int): Traversable[Disk] = {
    createRandomWithinBounds(Traversable.empty, amount, width, height)
  }

  @tailrec
  private def createRandomWithinBounds(current: Traversable[Disk], amount: Int, width: Int, height: Int): Traversable[Disk] = {
    if (current.size == amount) {
      current
    } else {
      createRandomWithinBounds(
        current ++ Traversable(createRandomWithinBounds(width, height)),
        amount,
        width,
        height
      )
    }
  }

  def createRandomWithinBounds(width: Int, height: Int): Disk = {
    val radius = Math.round(1 + (random.nextDouble() * Math.min(width, height) * 0.5)).toInt
    Disk(
      toNearestHalf(radius + (random.nextDouble() * (width - 2 * radius))),
      toNearestHalf(radius + (random.nextDouble() * (height - 2 * radius))),
      radius
    )
  }

  private def toNearestHalf(amount: Double): Double = {
    Math.round(amount / 0.5) * 0.5
  }
}