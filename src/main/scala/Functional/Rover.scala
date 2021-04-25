package Functional

object RoverPlatform {
  val MapWidth = 10
  val MapLength = 10
  val InitialCoords: (Int, Int) = (1, 1)
  val InitialHeading: Char = 'N'
  val MaximumMovement: Int = 10

  private type EitherState = Either[RoverError, RoverState]

  def initializeRover(c: (Int, Int) = InitialCoords, h: Char = InitialHeading): EitherState = {
    for {
      position <- createPosition(c)
      heading <- toHeading(h)
    } yield RoverState(position, heading)
  }

  def executeCommands(state: EitherState, cmds: List[Char]): EitherState = {
    cmds.foldLeft(state)((acc, cmd) => execute(acc, cmd))
  }

  private def execute(state: EitherState, cmd: Char): EitherState = {
    toCmd(cmd) match {
      case Right(cmd) => parseCommand(state, cmd)
      case Left(_) => Left(RoverError("Invalid Command"))
    }
  }

  private def parseCommand(state: EitherState, command: Command): EitherState = {
    command match {
      case Forward => move(state, Forward.multiplier)
      case Backward => move(state, Backward.multiplier)
      case LeftSide => state.map(s => updateHeading(s, s.heading.leftHeading))
      case RightSide => state.map(s => updateHeading(s, s.heading.rightHeading))
    }
  }

  private def updateHeading(s: RoverState, h: Heading): RoverState = RoverState(s.position, h, s.increment)

  private def move(state: EitherState, multiplier: Int): EitherState = {
    state match {
      case Right(s) =>
        val delta = calcDelta(s.heading.deltaUnit, multiplier, s.increment)
        updateCoords(s, delta)
      case Left(e) => Left(e)
    }
  }

  private def calcDelta(deltaUnit: (Int, Int), multiplier: Int, increment: Int): (Int, Int) = {
    val dx = deltaUnit._1 * multiplier * increment
    val dy = deltaUnit._2 * multiplier * increment
    (dx, dy)
  }

  private def updateCoords(state: RoverState, delta: (Int, Int)): EitherState = {
    val position = calculateCoordinates(state.position, delta._1, delta._2)
//     Here check if there is an obstacle or not. Return Left if there is.
    Right(RoverState(position, state.heading))
  }

  private def calculateCoordinates(p: Position, dx: Int, dy: Int): Position = {
    var pos = Position(p.x + dx, p.y + dy)
    if (pos.x > MapWidth) pos = wrapRight(pos)
    if (pos.x < 1) pos = wrapLeft(pos)
    if (pos.y > MapLength) pos = wrapTopDiagonally(pos)
    if (pos.y < 1) pos = wrapBottomDiagonally(pos)
    pos
  }

  private def createPosition(coords: (Int, Int)): Either[RoverError, Position] = {
    if (coords._1 > 0 && coords._1 <= MapWidth && coords._2 > 0 && coords._2 < MapLength) Right(Position(coords))
    else Left(RoverError("Invalid Initial Position"))
  }

  private def toHeading(h: Char): Either[RoverError, Heading] = {
    h match {
      case 'N' => Right(North)
      case 'S' => Right(South)
      case 'E' => Right(East)
      case 'W' => Right(West)
      case _ => Left(RoverError("Invalid Initial Heading"))
    }
  }

  private def toCmd(c: Char): Either[RoverError, Command] = {
    c match {
      case 'f' => Right(Forward)
      case 'b' => Right(Backward)
      case 'l' => Right(LeftSide)
      case 'r' => Right(RightSide)
      case _ => Left(RoverError("Invalid Command"))
    }
  }

  private def wrapRight(p: Position): Position = Position(p.x % MapWidth, p.y)
  private def wrapLeft(p: Position): Position = Position((p.x % MapWidth) + MapWidth, p.y)
  private def wrapTopDiagonally(p: Position): Position = Position((p.y + MapLength -1) % MapLength, p.x)
  private def wrapBottomDiagonally(p: Position): Position = Position((p.y % MapLength) + MapLength, p.x)
}

sealed trait Heading {
  def toString: String
  def leftHeading: Heading
  def rightHeading: Heading
  def deltaUnit: (Int, Int)
}
case object North extends Heading {
  override def toString = "North"
  override def leftHeading: Heading = West
  override def rightHeading: Heading = East
  override def deltaUnit: (Int, Int) = (0, 1)
}
case object South extends Heading {
  override def toString = "South"
  override def leftHeading: Heading = East
  override def rightHeading: Heading = West
  override def deltaUnit: (Int, Int) = (0, -1)
}
case object East extends Heading {
  override def toString = "East"
  override def leftHeading: Heading = North
  override def rightHeading: Heading = South
  override def deltaUnit: (Int, Int) = (-1, 0)
}
case object West extends Heading {
  override def toString = "West"
  override def leftHeading: Heading = South
  override def rightHeading: Heading = North
  override def deltaUnit: (Int, Int) = (1, 0)
}

sealed trait Lateral
sealed trait Medial {
  def multiplier: Int
}
sealed trait Command {
  def toString: String
}
case object LeftSide extends Command with Lateral {
  override def toString: String = "Left"
}
case object RightSide extends Command with Lateral {
  override def toString: String = "Right"
}
case object Forward extends Command with Medial {
  override def toString: String = "Forward"
  override def multiplier: Int = 1
}
case object Backward extends Command with Medial {
  override def toString: String = "Backward"
  override def multiplier: Int = -1
}

class InvalidCommand extends RuntimeException

case class Position(coordinates: (Int, Int) = (1, 1)) {
  def this(x: Int, y: Int) {
    this((x, y))
  }
  val x: Int = coordinates._1
  val y: Int = coordinates._2
}

case class RoverState(position: Position = Position(), heading: Heading = North, increment: Int = 1)

case class RoverError(message: String) extends Throwable
