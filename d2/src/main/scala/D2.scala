case class Action(input: Int, command: Command)
case class MovingState(horizontal: Int = 0, vertical: Int = 0, aim: Int = 0)

enum Command:
  case Forward extends Command
  case Up extends Command
  case Down extends Command

@main def entrypoint: Unit =
  val input = FileLoader.readFile("input.txt")
  val resultPart1 = stateProduct(interpret(input, interpretPart1))
  val resultPart2 = stateProduct(interpret(input, interpretPart2))
  println(s"result Part 1: $resultPart1")
  println(s"result Part 2: $resultPart2")

def interpret(commands: List[String], interpreter: (MovingState, Action) => MovingState): MovingState =
  commands.flatMap(parseAction).foldLeft(MovingState())(interpreter)

def interpretPart1(state: MovingState, action: Action): MovingState = action match
  case Action(value, Command.Forward) => state.copy(horizontal = state.horizontal + value)
  case Action(value, Command.Up)      => state.copy(vertical = state.vertical - value)
  case Action(value, Command.Down)    => state.copy(vertical = state.vertical + value)

def interpretPart2(state: MovingState, action: Action): MovingState = action match
  case Action(value, Command.Forward) =>
    state.copy(horizontal = state.horizontal + value, vertical = state.vertical + state.aim * value)
  case Action(value, Command.Up) =>
    state.copy(aim = state.aim - value)
  case Action(value, Command.Down) =>
    state.copy(aim = state.aim + value)

def stateProduct(state: MovingState): Int = state.horizontal * state.vertical

def parseAction(commandContent: String): Option[Action] =
  commandContent.split(" ") match
    case Array(command, value) =>
      command match
        case "forward" => Some(Action(value.toInt, Command.Forward))
        case "down"    => Some(Action(value.toInt, Command.Down))
        case "up"      => Some(Action(value.toInt, Command.Up))
        case _         => None
    case _ => None
