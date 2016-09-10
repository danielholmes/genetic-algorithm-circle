# Genetic Algorithm Circle

An implementation of the the problem outlined here: (http://www.ai-junkie.com/ga/intro/gat3.html) under the 
*Stuff to Try* section i.e.

    Given an area that has a number of non overlapping disks scattered about its surface... Use a genetic algorithm to 
    find the disk of largest radius which may be placed amongst these disks without overlapping any of them


## Dependencies

 - SBT
 - JDK 8+
 - Scala 2.11

To find available SBT dependency updates run `sbt dependencyUpdates`


## Tests

 - All: `sbt test`
 - Individual: `sbt "test-only org.danielholmes.gacirc.SurfaceSpec"`
 - Individual continuous: `sbt ~"test-only org.danielholmes.gacirc.SurfaceSpec"`


## Running

`sbt run`