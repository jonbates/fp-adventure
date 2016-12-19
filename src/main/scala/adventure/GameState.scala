package adventure

import ReturnType.ReturnState

sealed trait GameStatus
case object Redo extends GameStatus
case object GameRunning extends GameStatus
case object GameQuit extends GameStatus

case class PlayerState(loc: CellId, inv: List[Inventoryable], equipped: Set[Equipable] = Set(), camouflaged: Boolean = false, mp: Int = 15) {
  def swordEquipped = equipped.contains(Fragarach(false))
  def amuletEquipped = equipped.contains(IronAmulet())
}

case class GameState(player: PlayerState, map: WorldMap, status: GameStatus, creatures: Set[Creature]) {
  implicit def cellIdToCell(cellId: CellId): Cell = map.cellIdToCell(cellId)

  def redo = this.copy(status=Redo)

  def currentCell: Cell = player.loc

  def checkSpells: ReturnState =
    if (player.loc.groundAccess) {
      if (player.mp < 15) (List.empty[String], this.copy(player=player.copy(mp=15)))
      else (List.empty[String], this)
    }
    else if (player.camouflaged) {
      if (player.mp >= 5) {
        val newMp = player.mp - 5
        (List(s"Camouflage still working but down to ${newMp} magic points."),
         this.copy(player=player.copy(mp=newMp)))
      } else {
        (List("Camouflage off!  Not enough power."),
         this.copy(player=player.copy(camouflaged=false)))
      }
    }
    else (List.empty[String], this)

    def dropItem(item: Item with Inventoryable): GameState = {
      val cellInv = player.loc.inv
      val playerInv = player.inv

      val updatedState = this.copy(player=player.copy(inv=playerInv.filter(_ != item))).updateCurrentCell(
                                   player.loc.copy(inv=item :: cellInv))
                                                                      
      item match {
        case item: Equipable => updatedState.removeEquipped(item)
        case _ => updatedState
      }
    }
    
    def retrieveItem(item: Item with Inventoryable): GameState = {
      val cellInv = player.loc.inv
      val playerInv = player.inv
    
      this.copy(player=player.copy(inv=item :: playerInv)).updateCurrentCell(
                player.loc.copy(inv=cellInv.filter(_ != item)))
    }
    
    def updateMap(oldCell: Cell, newCell: Cell): GameState =
      this.copy(map=map.updateMap(oldCell, newCell))
    def updatePlayerLoc(loc: CellId): GameState =
      this.copy(player=player.copy(loc=loc))
    def updateCurrentCell(newCell: Cell): GameState =
      updateMap(player.loc, newCell)
    def addEquipped(item: Equipable): GameState =
      this.copy(player=player.copy(equipped=player.equipped + item))
    def removeEquipped(item: Equipable): GameState =
      this.copy(player=player.copy(equipped=player.equipped - item))
    def addMapPath(cellId: CellId, newPath: (Direction, CellId)): GameState =
      this.copy(map=map.addPath(cellId, newPath))
}

object GameState {
  def death(text: List[String]): ReturnState =
    (text ::: List("\nYou awake in your bed.  Wow, bad dream!"),
     GameState.initialState)

  def initialState: GameState = {
     val (worldMap, playerStart) = WorldMap.initialState
     GameState(PlayerState(playerStart, List(IronAmulet())),  //playerStart // WorldMap.street6
               worldMap,
               GameRunning,
               Set(Loki(WorldMap.forest4), Diana(WorldMap.street6)))
      }
}
