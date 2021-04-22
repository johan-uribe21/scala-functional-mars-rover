package Functional

import scala.annotation.tailrec

object RoverPlatform {
  val MapWidth = 10
  val MapLength = 10

  def initializeRover(coords: (Int, Int), heading: Char): Option[RoverState] = {
    Some(RoverState(Position(coords), Heading(heading)))
  }

  @tailrec
  def executeCommands(state: Option[RoverState], cmds: List[Char]): Option[RoverState] = {
    if (cmds.isEmpty) return state
    val newState = execute(state, Command(cmds.head))
    executeCommands(newState, cmds.tail)
  }

  private def execute(state: Option[RoverState], command: Command): Option[RoverState] = {
    command match {
      case Command.Forward => move(state, state.increment)
      case Command.Backward => move(state, -state.increment)
      case Command.Left => turnLeft(state)
      case Command.Right => turnRight(state)
      case _ => throw new InvalidCommand
    }
  }

  private def turnLeft(state: Option[RoverState]): Option[RoverState] = {
    state.heading match {
      case Heading.North => updateHeading(state, Heading.West)
      case Heading.South => updateHeading(state, Heading.East)
      case Heading.East => updateHeading(state, Heading.North)
      case Heading.West => updateHeading(state, Heading.South)
      case _ => throw new InvalidCommand
    }
  }

  private def turnRight(state: Option[RoverState]): Option[RoverState] = {
    state.heading match {
      case Heading.North => updateHeading(state, Heading.East)
      case Heading.South => updateHeading(state, Heading.West)
      case Heading.East => updateHeading(state, Heading.South)
      case Heading.West => updateHeading(state, Heading.North)
      case _ => throw new InvalidCommand
    }
  }

  private def updateHeading(state: Option[RoverState], h: Heading): Option[RoverState] = RoverState(state.position, h)

  private def move(state: Option[RoverState], i: Int): Option[RoverState] = {
    state match {
      case Some(state) => state.heading match {
        case Heading.North => updateCoords(state, dy = i)
        case Heading.South => updateCoords(state, dy = -i)
        case Heading.East => updateCoords(state, dx = i)
        case Heading.West => updateCoords(state, dx = -i)
        case _ => throw new InvalidCommand
      }
      case None => None
    }
  }

  private def updateCoords(state: Option[RoverState], dx: Int = 0, dy: Int = 0): Option[RoverState] = {
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