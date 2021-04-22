package marsrover

import org.scalatest.{BeforeAndAfter, WordSpec}

class RoverShould extends WordSpec with BeforeAndAfter {

  private var rover: Rover = _
  private val startingCoords = (1, 1)

  before {
    rover = new Rover(startingCoords, 'N')
  }

  "initialize with starting point and heading" in {
    assert(rover.position.toTuple == startingCoords)
    assert(rover.heading == Heading.North)
  }

  "receive and execute commands - empty" in {
    rover.execute(Array())

    assert(rover.position.toTuple == startingCoords)
    assert(rover.heading == Heading.North)
  }

  "move forward heading North" in {
    rover.execute(Array('f'))

    assert(rover.position.toTuple == (1, 2))
    assert(rover.heading == Heading.North)
  }

  "move backward heading North" in {
    rover.execute(Array('b'))

    assert(rover.position.toTuple == (1, 0))
    assert(rover.heading == Heading.North)
  }

  "rotate left in place" in {
    rover.execute(Array('l'))

    assert(rover.position.toTuple == (1, 1))
    assert(rover.heading == Heading.West)
  }

  "rotate right in place" in {
    rover.execute(Array('r'))

    assert(rover.position.toTuple == (1, 1))
    assert(rover.heading == Heading.East)
  }
}
