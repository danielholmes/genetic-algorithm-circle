package org.danielholmes.gacirc

case class Config(
  surface: Surface,
  crossoverRate: Double,
  mutationRate: Double,
  populationSize: Int
)
