package marsrover

class Rover(startingCoords: (Int, Int), startHeading: Char, stepSize: Int = 1) {

  var position: Position = Position(startingCoords._1, startingCoords._2)
  var heading: Heading = Heading(startHeading)

  def execute(commands: Array[Char]): Unit = commands.foreach(cmd => executeCommand(Direction(cmd)))

  private def executeCommand(dir: Direction): Unit = {
    dir match {
      case Direction.Forward => moveSteps(stepSize)
      case Direction.Backward => moveSteps(-stepSize)
      case Direction.Left => rotateLeft()
      case Direction.Right => rotateRight()
      case _ => ()
    }
  }

  private def rotateLeft(): Unit = {
    heading match {
      case Heading.North => updateHeading(Heading.West)
      case Heading.South => updateHeading(Heading.East)
      case Heading.East => updateHeading(Heading.North)
      case Heading.West => updateHeading(Heading.South)
      case _ => throw new InvalidCommand
    }
  }
  private def rotateRight(): Unit = {
    heading match {
      case Heading.North => updateHeading(Heading.East)
      case Heading.South => updateHeading(Heading.West)
      case Heading.East => updateHeading(Heading.South)
      case Heading.West => updateHeading(Heading.North)
      case _ => throw new InvalidCommand
    }
  }

  private def updateHeading(newHeading: Heading): Unit = heading = newHeading

  private def moveSteps(steps: Int = 1): Unit = {
    heading match {
      case Heading.North => updateCoordinates(delY = steps)
      case Heading.South => updateCoordinates(delY = -steps)
      case Heading.East => updateCoordinates(delX = steps)
      case Heading.West => updateCoordinates(delX = -steps)
      case _ => throw new InvalidCommand
    }
  }

  private def updateCoordinates(delX: Int = 0, delY: Int = 0): Unit = {
    val newPosition = Position(position.x + delX, position.y + delY)
    position = newPosition
  }
}

case class Heading(value: Char)
object Heading {
  val North: Heading = Heading('N')
  val South: Heading = Heading('S')
  val East: Heading = Heading('E')
  val West: Heading = Heading('W')
}

case class Direction(dir: Char)
object Direction {
  val Forward: Direction = Direction('f')
  val Backward: Direction = Direction('b')
  val Left: Direction = Direction('l')
  val Right: Direction = Direction('r')
}

class InvalidCommand extends RuntimeException

case class Position(x: Int, y: Int) {
  def toTuple: (Int, Int) = (x, y)
}