package aoc20
package day12

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    readInputLines(12)
      .flatMap { lines =>
        val instructions = lines.map(Instruction.fromString)
        for
          _ <- Console.output(1, moveShip(instructions, DirectShip()))
          _ <- Console.output(2, moveShip(instructions, WayPointShip()))
        yield ()
      }
      .as(ExitCode.Success)

  def moveShip(instructions: Vector[Instruction], ship: Ship) =
    instructions.foldLeft(ship)(_.move(_)).manhatten

sealed trait Orientation:
  def rotate(rotation: Rotation) =
    val order = Orientation.Order
    val b = order.indexOf(this)
    order((rotation.rightAngles + b) % 4)

case object Orientation:
  final private val Order = Vector(North, East, South, West)
case object North extends Orientation
case object South extends Orientation
case object East extends Orientation
case object West extends Orientation

sealed trait Instruction

object Instruction:
  def fromString(s: String) =
    val (c, a) = s.splitAt(1)
    val i = a.toInt
    c match
      case "N" => MoveAbsolute(North, i)
      case "S" => MoveAbsolute(South, i)
      case "E" => MoveAbsolute(East, i)
      case "W" => MoveAbsolute(West, i)
      case "L" => Rotation(Left, i)
      case "R" => Rotation(Right, i)
      case "F" => MoveForward(i)

sealed trait LeftRight
case object Left extends LeftRight
case object Right extends LeftRight

case class Rotation(lr: LeftRight, degrees: Int) extends Instruction:
  assert(degrees % 90 == 0)

  def rightAngles =
    if lr == Right then degrees / 90
    else 4 - (degrees / 90)

case class MoveAbsolute(orientation: Orientation, amount: Int)
    extends Instruction

case class MoveForward(amount: Int) extends Instruction

trait Ship:
  def east: Int
  def north: Int

  def move(instruction: Instruction): Ship
  def manhatten = math.abs(east) + math.abs(north)

case class DirectShip(
  orientation: Orientation = East,
  east: Int = 0,
  north: Int = 0,
) extends Ship:

  def move(instruction: Instruction): Ship = instruction match
    case r: Rotation => copy(orientation = orientation.rotate(r))
    case MoveAbsolute(orientation, amount) => move(orientation, amount)
    case MoveForward(amount) => move(orientation, amount)

  private def move(orientation: Orientation, amount: Int): Ship =
    orientation match
      case North => copy(north = north + amount)
      case South => copy(north = north - amount)
      case East => copy(east = east + amount)
      case West => copy(east = east - amount)

case class WayPoint(
  east: Int = 10,
  north: Int = 1,
):
  def move(orientation: Orientation, amount: Int): WayPoint =
    orientation match
      case North => copy(north = north + amount)
      case South => copy(north = north - amount)
      case East => copy(east = east + amount)
      case West => copy(east = east - amount)

  def rotate(r: Rotation) = r.rightAngles match
    case 0 => this
    case 1 => copy(east = north, north = -east)
    case 2 => copy(east = -east, north = -north)
    case 3 => copy(east = -north, north = east)
case class WayPointShip(
  east: Int = 0,
  north: Int = 0,
  waypoint: WayPoint = WayPoint(),
) extends Ship:

  def move(instruction: Instruction): Ship = instruction match
    case r: Rotation => copy(waypoint = waypoint.rotate(r))
    case MoveAbsolute(orientation, amount) =>
      copy(waypoint = waypoint.move(orientation, amount))
    case MoveForward(times) =>
      copy(
        east = waypoint.east * times + east,
        north = waypoint.north * times + north,
      )
