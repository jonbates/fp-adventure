package adventure

object ReturnType {
  type ReturnState = (List[String], GameState)
}
import ReturnType.ReturnState

case class GameOps(
    state: GameState,
    ops: List[GameState => ReturnState] = Nil) {
  def add(line: String): GameOps = add(List(line))
  def add(lines: List[String]): GameOps =
    add((state: GameState) => (lines, state))
  def addStringList(f: GameState => List[String]) =
    add((state: GameState) => (f(state), state))
  def addState(f: GameState => GameState): GameOps = {
    add((state: GameState) => (List.empty[String], f(state)))
  }
  def add(f: GameState => ReturnState): GameOps =
    this.copy(ops=f :: ops)
  
  def run: IO[GameState] = ops.reverse.foldLeft(IO.point(state)) {
    case (stateIO, ops) =>
      for {
        oldState <- stateIO
        (text, state) = oldState.status match {
          case Redo | GameQuit => (List.empty[String], oldState)
          case GameRunning => ops(oldState)
        }
        _ <- IO.textToIO(text)
      } yield state
  }
}

object Application {
  def parse(text: String): Either[String, Command] = Command.parse(text)

  def act(command: Command, oldState: GameState): ReturnState = {
    val GameState(player, map, status, creatures) = oldState
    command match {
      case Quit =>
        (List("\nThanks for playing!"), oldState.copy(status=GameQuit))
      case Inspect(item) => item match {
        case Left(unparsed) =>
          (List(s"Unable to find $unparsed to inspect"), oldState.redo)
        case Right(item) => item.inspect(oldState)
      }
      case Inventory =>  // TODO add equip info
         (List(player.inv.map(_.describe).mkString(
           "Your inventory contains: ", ", ", ".")), oldState.redo)
      case Drop(item) => item match {
        case Left(unparsed) =>
          (List(s"I don't see $unparsed in my inventory to drop"), oldState.redo)
        case Right(item) => item.drop(oldState)
      }
      case Get(item) => item match {
        case Left(unparsed) =>
          (List(s"I'm not sure what a $unparsed is."), oldState.redo)
        case Right(item) => item.retrieve(oldState)
      }
      case Equip(item) => item match {
        case Left(unparsed) =>
          (List(s"I'm not sure what a $unparsed is."), oldState.redo)
        case Right(item) => item.equip(oldState)
      }
      case Go(direction) =>
        map.move(player.loc, direction) match {
          case None => (List("\nUnable to move that direction!"), oldState.redo)
          case Some(cell) => (List(s"Moving ${direction}"),
                              oldState.copy(player=player.copy(loc=cell)))
        }
      case Attack(creature) => creature match {
        case Left(unparsed) =>
          (List(s"Cannot find ${unparsed} to attack!\n"), oldState.redo)
        case Right(creature) =>
          creatures.filter(_ == creature).headOption match {
            case None => (List.empty[String], oldState)
            case Some(creature) =>
              if (creature.loc == player.loc) creature.attacked(oldState)
              else (List("That creature isn't here!"), oldState.redo)
           }
       }
       case Cast(spell) => spell match {
         case Left(unparsed) =>
           (List(s"I don't know the spell ${unparsed}!\n"), oldState.redo)
         case Right(Camo) =>
           (List("Casting camo!",
                 "You should stay in contact with the ground to maintain camo!"),
            oldState.copy(player=player.copy(camouflaged=true)))
         case Right(Portal) =>
           if (oldState.currentCell.inv.contains(TetheredTree()))
             (List("Let's get out of here!  Adios Diana!  Hello Tir na nOg!",
                   "Congrats!!! You've beaten the game!"),
              oldState.copy(status=GameQuit))
           else (List("You need a tethered tree around to cast portal."),
                 oldState.redo)
       }
       case Help =>
         (List("********** Commands *********",
              "inv -- inventory",
              "inspect <item, creature>",
              "drop <item>",
              "take <item>",
              "attack <creature>",
              "go <north, south, east, west>",
              "equip <item>",
              "cast <camo, portal>",
              "quit",
              "help",
              "******************************"), oldState.redo)
    }
  }

  def loop(state: GameState): IO[GameState] = {
    for {
      newState <- GameOps(state)
        .addStringList((state) => state.currentCell.getDescription)
        .addState((state) =>
          if (!state.currentCell.firstEnter.isEmpty) {
            val cell = state.currentCell.copy(firstEnter=None)
            state.updateCurrentCell(cell)
          } else state)
        .add(state => state.checkSpells)
        .addStringList(Creature.getDescription _)
        .addStringList((state) => state.map.directions(state.player.loc))
        .run

      _ <- Console.putStr(": ")
      input <- Console.getStrLn

      finalState <- GameOps(newState)
        .add("*" * 60 + "\n")
        .add(curState => parse(input) match {
            case Left(error) => (List(error), state.redo)  // the state at the start of loop
            case Right(command) => act(command, curState)
          } )
        .add(Creature.makeAttacks _)
        .add(Creature.makeMoves _)
        .run

      newState <- finalState.status match {
        case GameQuit => IO.point(state)
        case GameRunning => loop(finalState)
        case Redo => loop(state)
      }

    } yield newState
  }

  /**
   * A pure IO value representing the application.
   */
  def start: IO[Unit] =
    for {
      gameState <- loop(GameState.initialState)
    } yield ()
}

object Main {
  /**
   * The main function of the application, which performs the effects
   * described by the application's IO monad.
   */
  def main(args: Array[String]): Unit = Application.start.unsafePerformIO()
}
