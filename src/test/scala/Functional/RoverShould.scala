package Functional

import org.scalatest.{BeforeAndAfter, EitherValues, WordSpec}
import RoverPlatform._
import org.scalatest.Matchers.{convertToAnyShouldWrapper, matchPattern}

class RoverShould extends WordSpec with BeforeAndAfter with EitherValues {
  private val InitialCoords = (1, 1)
  private val InitialHeading = 'N'
  private var roverState = initializeRover(InitialCoords, InitialHeading)

  before {
    roverState = initializeRover(InitialCoords, InitialHeading)
  }

  "initialize with a starting point and heading" in  {
    roverState should matchPattern { case Right(RoverState(Position(InitialCoords), North, 1)) => }
  }

  "receive and execute commands - Empty" in {
    roverState = executeCommands(roverState, List())

    roverState should matchPattern { case Right(RoverState(Position(InitialCoords), North, 1)) => }
  }

  "move forward one unit when given command 'f'" in {
    roverState = executeCommands(roverState, List('f'))
    val NewCoords = (1, 2)
    roverState should matchPattern { case Right(RoverState(Position(NewCoords), North, 1)) => }
  }

  "move backwards one unit when given command 'b'" in {
    roverState = executeCommands(roverState, List('b'))
    val NewCoords = (10, 1)
    roverState should matchPattern { case Right(RoverState(Position(NewCoords), North, 1)) => }
  }

  "head west when given the command 'l'" in {
    roverState = executeCommands(roverState, List('l'))
    roverState should matchPattern { case Right(RoverState(Position(InitialCoords), West, 1)) => }
  }

  "head east when given the command 'r'" in {
    roverState = executeCommands(roverState, List('r'))
    roverState should matchPattern { case Right(RoverState(Position(InitialCoords), East, 1)) => }

  }

  "move forward two units when given the commands ('f', 'f')" in {
    roverState = executeCommands(roverState, List('f', 'f'))
    val NewCoords = (1, 3)

    roverState should matchPattern { case Right(RoverState(Position(NewCoords), North, 1)) => }
  }

  "move north 1 unit, turn east, then move east 1 unit when given the commands ('f', 'r', 'f')" in {
    roverState = executeCommands(roverState, List('f', 'r', 'f'))
    val NewCoords = (2, 2)

    roverState should matchPattern { case Right(RoverState(Position(NewCoords), East, 1)) => }
  }

  "grid system wraps around top-to-bottom diagonally when moving north 11 units" in {
    val commands = List.fill(11)('f')
    roverState = executeCommands(roverState, commands)
    val NewCoords = (10, 2)

    roverState should matchPattern { case Right(RoverState(Position(NewCoords), North, 1)) => }
  }

  "grid system wraps around bottom-to-top when moving south 1 unit" in {
    roverState = executeCommands(roverState, List('b'))
    val NewCoords = (10, 1)

    roverState should matchPattern { case Right(RoverState(Position(NewCoords), North, 1)) => }

  }

  "grid system wraps around right-to-left when moving east 11 units" in {
    val commands = 'r' :: List.fill(11)('f')
    roverState = executeCommands(roverState, commands)
    val NewCoords = (2, 1)

    roverState should matchPattern { case Right(RoverState(Position(NewCoords), East, 1)) => }
  }

  "grid system wraps around left-to-right when moving west 1 unit" in {
    val commands = List('l', 'f')
    roverState = executeCommands(roverState, commands)
    val NewCoords = (10, 1)

    roverState should matchPattern { case Right(RoverState(Position(NewCoords), West, 1)) => }
  }

  "rover stops when it hits an obstacle at (3, 2)" in {
    val commands = List('f', 'r', 'f', 'f')
    roverState = executeCommands(roverState, commands)
    val ErrorMsg = "Collision occurred at (3, 2)"

    roverState should matchPattern { case Left(RoverError(ErrorMsg)) => }
  }
}
