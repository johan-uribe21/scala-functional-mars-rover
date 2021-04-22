package Functional

import org.scalatest.{BeforeAndAfter, WordSpec}
import RoverPlatform._

class RoverShould extends WordSpec with BeforeAndAfter {
  private val initialCoords = (1, 1)
  private val initialHeading = 'N'
  private var roverState = initializeRover(initialCoords, initialHeading)

  before {
    roverState = initializeRover(initialCoords, initialHeading)
  }

  "initialize with a starting point and heading" in  {
    assert(roverState.position == Position(initialCoords))
    assert(roverState.heading == Heading(initialHeading))
  }

  "receive and execute commands - Empty" in {
    roverState = executeCommands(roverState, List())

    assert(roverState.position == Position(initialCoords))
    assert(roverState.heading == Heading(initialHeading))
  }

  "move forward one unit when given command 'f'" in {
    roverState = executeCommands(roverState, List('f'))
    val newCoords = (1, 2)

    assert(roverState.position == Position(newCoords))
    assert(roverState.heading == Heading(initialHeading))
  }

  "move backwards one unit when given command 'b'" in {
    roverState = executeCommands(roverState, List('b'))
    val newCoords = (10, 1)

    assert(roverState.position == Position(newCoords))
    assert(roverState.heading == Heading(initialHeading))
  }

  "head west when given the command 'l'" in {
    roverState = executeCommands(roverState, List('l'))
    assert(roverState.position == Position(initialCoords))
    assert(roverState.heading == Heading.West)
  }

  "head east when given the command 'r'" in {
    roverState = executeCommands(roverState, List('r'))
    assert(roverState.position == Position(initialCoords))
    assert(roverState.heading == Heading.East)
  }

  "move forward two units when given the commands ('f', 'f')" in {
    roverState = executeCommands(roverState, List('f', 'f'))
    val newCoords = (1, 3)

    assert(roverState.position == Position(newCoords))
    assert(roverState.heading == Heading(initialHeading))
  }

  "move north 1 unit, turn east, then move east 1 unit when given the commands ('f', 'r', 'f')" in {
    roverState = executeCommands(roverState, List('f', 'r', 'f'))
    val newCoords = (2, 2)

    assert(roverState.position == Position(newCoords))
    assert(roverState.heading == Heading.East)
  }

  "grid system wraps around top-to-bottom diagonally when moving north 11 units" in {
    val commands = List.fill(11)('f')
    roverState = executeCommands(roverState, commands)
    val newCoords = (10, 2)

    assert(roverState.position == Position(newCoords))
    assert(roverState.heading == Heading.North)
  }

  "grid system wraps around bottom-to-top when moving south 1 unit" in {
    roverState = executeCommands(roverState, List('b'))
    val newCoords = (10, 1)

    assert(roverState.position == Position(newCoords))
    assert(roverState.heading == Heading.North)
  }

  "grid system wraps around right-to-left when moving east 11 units" in {
    val commands = 'r' :: List.fill(11)('f')
    roverState = executeCommands(roverState, commands)
    val newCoords = (2, 1)

    assert(roverState.position == Position(newCoords))
    assert(roverState.heading == Heading.East)
  }

  "grid system wraps around left-to-right when moving west 1 unit" in {
    val commands = List('l', 'f')
    roverState = executeCommands(roverState, commands)
    val newCoords = (10, 1)

    assert(roverState.position == Position(newCoords))
    assert(roverState.heading == Heading.West)
  }

  "rover stops when it hits an obstacle" in {

  }
}
