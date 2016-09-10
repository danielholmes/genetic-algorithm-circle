package org.danielholmes.gacirc

import scala.swing.event.Event

case class RestartRequested(initialPopulation: Traversable[Disk]) extends Event
