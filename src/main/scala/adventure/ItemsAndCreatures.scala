package adventure

import ReturnType.ReturnState
import Cell.cellToCellId  // implicit conversion from Cell to CellId

trait Describable[A] {
  def describe: String
}

sealed trait Inventoryable extends Item {
  def retrieve(state: GameState): ReturnState =
    if (state.currentCell.inv.contains(this)) {
      val newState = state.retrieveItem(this)
      this match {
        case _: Fragarach => (List("You pick up Fragarach! This'll be fun."), newState)
        case _: IronAmulet => (List("You pick up your amulet!  Why did you ever set that down!"), newState)
      }
    } else (List(s"I don't see ${this.describe}."), state.redo)
  
  def drop(state: GameState): ReturnState =
    if (state.player.inv.contains(this)) {
      (List(s"You dropped an ${this.describe}."), state.dropItem(this))
    } else (List(s"You don't have ${this.describe} to drop."), state.redo)
}

sealed trait Equipable extends Inventoryable {
  def equip(state: GameState): ReturnState =
    if (state.player.inv.contains(this)) {
      val newState = state.addEquipped(this)
      this match {
        case _: Fragarach => (List("Fragarach at the ready!"), newState)
        case _: IronAmulet => (List("I feel safer already!"), newState)
      }
    } else (List(s"I don't see that in inventory."), state.redo)
}

trait Inspectable[A] {
  def inspect(state: GameState): ReturnState
}

sealed trait Hidable {
  def isHidden: Boolean = false
}

sealed trait Item extends Hidable

sealed trait Spell
case object Camo extends Spell
case object Portal extends Spell

case class TetheredTree() extends Item
case class Fragarach(override val isHidden: Boolean) extends Item with Inventoryable with Equipable
case class MorriganNote() extends Item
case class IronAmulet() extends Item with Inventoryable with Equipable
case class Planter() extends Item

object Item {
  implicit class DescribableItem(item: Item) extends Describable[Item] {
    def describe = item match {
      case _: TetheredTree => "a tree tethered to Tir na nOg"
      case _: Fragarach => "a sword"
      case _: MorriganNote => "a note"
      case _: IronAmulet => "an iron amulet"
      case _: Planter => "a planter with herbs"
    }
  }
  
  implicit class InspectableItem(item: Item) extends Inspectable[Item] {
    def inspect(state: GameState): ReturnState =
      if ((state.player.inv.toSet ++ state.currentCell.inv).contains(item))
        item match {
          case _: TetheredTree => (List("You should be able to cast portal to escape"),
                                   state)
          case _: Fragarach => (List("An unstoppable sword"), state)
          case _: MorriganNote => (List("You need to find a tethered tree.  The huntress Diana is coming..."), state)
          case _: IronAmulet => (List("An iron amulet with several charms attached"), state)
          case _: Planter => (List("A large planter with several herbs that has been holding something sharp"),
                              planterChangeState(state))
        }
      else
        (List(s"Cannot find ${item.describe} to inspect!\n"), state.redo)
      
    def planterChangeState(gameState: GameState): GameState = {
      val cell = gameState.currentCell
      if (cell.inv.contains(Fragarach(true))) {
        val updatedInv = cell.inv.map { case Fragarach(true) => Fragarach(false); case x => x }
        val updatedCell = cell.copy(inv=updatedInv)
        gameState.updateCurrentCell(updatedCell)
      } else gameState
    }
  }
}

sealed trait Creature {
  def updateLocation(loc: CellId): Creature = this match {
    case loki: Loki => loki.copy(loc = loc)
    case diana: Diana => diana.copy(loc = loc)
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: Creature => that.describe == this.describe
      case _ => false
  }

  def loc: CellId
  def move(state: GameState): ReturnState = (List.empty[String], state)
  def attack(state: GameState): ReturnState = (List.empty[String], state)
  def defeated(state: GameState): GameState = state
  def attacked(state: GameState): ReturnState =
    (List(s"You can't attack ${this.describe}."), state)
}

case class Loki(loc: CellId) extends Creature {
  override def defeated(state: GameState): GameState =
    state.addMapPath(loc, (North -> WorldMap.forest5)).copy(creatures=state.creatures - this)

  override def attack(state: GameState): ReturnState =
    if(loc == state.player.loc) {
      val text = List("", "Loki casts hel-fire against you!")
      if (state.player.amuletEquipped) (text :+ "Phew! That was a little warm.", state)
      else GameState.death(text :+ "You're toast!  I'd recommend protection next time!")
    }
    else (List.empty[String], state)

  override def attacked(state: GameState): ReturnState =
    if (state.player.swordEquipped)
      (List(s"Loki takes a nasty gash to the leg.  He escapes to the sky as a fireball!"),
       defeated(state))
    else GameState.death(
        List(s"You attack Loki with your bare hands, yet he is basically made of fire!",
             "Bad Idea!  You're toast!"))
}

case class Diana(loc: CellId) extends Creature {
  override def move(state: GameState): ReturnState = {
    if (state.player.loc == loc) (List.empty[String], state)
    else state.map.getPrefMove(loc, List(West, North)) match {
      case None => (List.empty[String], state)
      case Some(loc) => (List.empty[String], state.copy(creatures=state.creatures - this + this.copy(loc=loc)))
    }
  }

  val r = scala.util.Random

  override def defeated(state: GameState): GameState =
    state.copy(creatures=state.creatures - this + Diana(WorldMap.street1))

  override def attack(state: GameState): ReturnState =
    if(loc == state.player.loc) {
      val text = List("", "Diana fires her bow!")
      if (state.player.camouflaged) {
        if (r.nextFloat > 0.10f) (text :+ "A narrow miss!  That camo helps!", state)
        else GameState.death(text :+ "Not hidden well enough.  Diana's arrow takes you out.")
      }
      else {
        if (r.nextFloat > 0.75f)
          (text :+ "You're hit, but it's not life-threatening for a druid.  You'd be safer if you were more hidden",
           state)
        else GameState.death(text :+ "Dead on contact!  Maybe try being a little sneakier next time.")
      }
    }
    else (List.empty[String], state)

  override def attacked(state: GameState): ReturnState  =
    if (state.player.swordEquipped)
      if (state.player.camouflaged)
        (List(s"Off with her head!  Nicely done.  Though she does regenerate."),
         defeated(state))
      else
        (List(s"You deliver a flesh wound.  She saw you coming from a mile away."),
         state)
    else
      (List(s"You grapple with Diana, but it does neither of you damage."),
       state)
}

object Creature {
  implicit class DescribeCreature(creature: Creature) extends Describable[Creature] {
    def describe: String = creature match {
      case loki: Loki => "Loki the shape shifting god"
      case diana: Diana => "Diana the godess of the hunt"
    }
  }
  
  implicit class InspectCreature(creature: Creature) extends Inspectable[Creature] {
    def inspect(state: GameState): ReturnState =
      if (state.creatures.filter(_.loc == state.player.loc).contains(creature))
        creature match {
          case _: Loki =>
            (List("Loki has a dangerous fire attack that you need to protect against"), state)
          case _: Diana =>
            (List("Diana can be killed but she will regenerate, so stealth may help"), state)
        }
      else
        (List(s"Cannot find ${creature.describe} to inspect!\n"), state.redo)
  }
  
  def performAction(state: GameState, f: (Creature, GameState) => ReturnState) =
    state.creatures.foldLeft((List.empty[String], state)) {
        case ((list, state), creature) => {
          val (newList, newState) = f(creature, state)
          (list ::: newList, newState)
        }
    }
  
  def makeAttacks(state: GameState): ReturnState =
    performAction(state, (creature, gameState) => creature.attack(gameState))
    
  def makeMoves(state: GameState): ReturnState =
    performAction(state, (creature, gameState) => creature.move(gameState))

  def getDescription(state: GameState): List[String] =
      state.creatures.filter(_.loc == state.player.loc).toList match {
        case Nil => List.empty[String]
        case list =>
          List(list.map(_.describe).mkString("You've encountered: ", ", ", ".\n"))
      }
}
