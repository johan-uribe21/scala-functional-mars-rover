package Functional

import scala.annotation.tailrec

object RoverPlatform {
  val MapWidth = 10
  val MapLength = 10

  def initializeRover(coords: (Int, Int), heading: Char): RoverState = {
    RoverState(Position(coords), Heading(heading))
  }

  @tailrec
  def executeCommands(state: RoverState, cmds: List[Char]): RoverState = {
    if (cmds.isEmpty) return state
    val newState = execute(state, Command(cmds.head))
    executeCommands(newState, cmds.tail)
  }

  private def execute(state: RoverState, command: Command): RoverState = {
    command match {
      case Command.Forward => move(state, state.increment)
      case Command.Backward => move(state, -state.increment)
      case Command.Left => turnLeft(state)
      case Command.Right => turnRight(state)
      case _ => throw new InvalidCommand
    }
  }

  private def turnLeft(state: RoverState): RoverState = {
    state.heading match {
      case Heading.North => updateHeading(state, Heading.West)
      case Heading.South => updateHeading(state, Heading.East)
      case Heading.East => updateHeading(state, Heading.North)
      case Heading.West => updateHeading(state, Heading.South)
      case _ => throw new InvalidCommand
    }
  }

  private def turnRight(state: RoverState): RoverState = {
    state.heading match {
      case Heading.North => updateHeading(state, Heading.East)
      case Heading.South => updateHeading(state, Heading.West)
      case Heading.East => updateHeading(state, Heading.South)
      case Heading.West => updateHeading(state, Heading.North)
      case _ => throw new InvalidCommand
    }
  }

  private def updateHeading(state: RoverState, h: Heading): RoverState = RoverState(state.position, h)

  private def move(state: RoverState, i: Int): RoverState = {
    state.heading match {
      case Heading.North => updateCoords(state, dy = i)
      case Heading.South => updateCoords(state, dy = -i)
      case Heading.East => updateCoords(state, dx = i)
      case Heading.West => updateCoords(state, dx = -i)
      case _ => throw new InvalidCommand
    }
  }

  private def updateCoords(state: RoverState, dx: Int = 0, dy: Int = 0): RoverState = {
    val position = calculateCoordinates(state.position, dx, dy)
    RoverState(position, state.heading)
  }

  private def calculateCoordinates(position: Position, dx: Int, dy: Int): Position = {
    var pos = Position(position.x + dx, position.y + dy)
    if (pos.x > MapWidth) pos = wrapRight(pos)
    if (pos.x < 1) pos = wrapLeft(pos)
    if (pos.y > MapLength) pos = wrapTopDiagonally(pos)
    if (pos.y < 1) pos = wrapBottomDiagonally(pos)
    pos
  }

  private def wrapRight(p: Position): Position = Position(p.x % MapWidth, p.y)
  private def wrapLeft(p: Position): Position = Position((p.x % MapWidth) + MapWidth, p.y)
  private def wrapTopDiagonally(p: Position): Position = Position((p.y + MapLength -1) % MapLength, p.x)
  private def wrapBottomDiagonally(p: Position): Position = Position((p.y % MapLength) + MapLength, p.x)
}

case class Heading(value: Char)
object Heading {
  val North: Heading = Heading('N')
  val South: Heading = Heading('S')
  val East: Heading = Heading('E')
  val West: Heading = Heading('W')
}

case class Command(dir: Char)
object Command {
  val Forward: Command = Command('f')
  val Backward: Command = Command('b')
  val Left: Command = Command('l')
  val Right: Command = Command('r')
}

class InvalidCommand extends RuntimeException

case class Position(coordinates: (Int, Int) = (1, 1)) {
  def this(x: Int, y: Int) {
    this((x, y))
  }
  val x: Int = coordinates._1
  val y: Int = coordinates._2
}

case class RoverState(position: Position, heading: Heading, increment: Int = 1)