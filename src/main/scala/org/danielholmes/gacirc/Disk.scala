package org.danielholmes.gacirc

case class Disk(centreX: Double, centreY: Double, radius: Double) {
  require(centreX >= 0)
  require(centreY >= 0)
  require(radius > 0)

  lazy val topY = centreY - radius
  lazy val bottomY = centreY + radius
  lazy val leftX = centreX - radius
  lazy val rightX = centreX + radius

  def overlaps(other: Disk): Boolean = {
    centreToCentreDistance(other) < (radius + other.radius)
  }

  private def centreToCentreDistance(other: Disk): Double = {
    Math.sqrt(Math.pow(centreX - other.centreX, 2) + Math.pow(centreY - other.centreY, 2))
  }
}
