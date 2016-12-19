package adventure

class CellId
object CellId {
  def apply() = new CellId
}

case class Cell(
    cellId: CellId,
    inv: List[Item],
    shortDesc: String,
    baseDesc: String,
    groundAccess: Boolean = false,
    firstEnter: Option[String] = None) {
  def describe = baseDesc
  def getDescription = List((firstEnter match {
    case None => "\n"
    case Some(line) => line + "\n\n"
  })  + baseDesc + (if (getItems == "") "\n" else getItems))

  private def getItems = inv.filter(!_.isHidden) match {
    case Nil => ""
    case list => list.map(_.describe).mkString("\n\nYou see: ", ", ", ".\n")
  }
}

object Cell {
  implicit def cellToCellId(cell: Cell): CellId = cell.cellId
}

sealed trait Direction {
  override def toString = this match {
    case North => "north"
    case South => "south"
    case East => "east"
    case West => "west"
  }
}
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction

case class WorldMap(
    private val value: Map[CellId, Map[Direction, CellId]],
    private val cellIdToCellMap: Map[CellId, Cell]) {
  implicit def cellIdToCell(cellId: CellId): Cell = cellIdToCellMap(cellId)

  def getPrefMove(cellId: CellId, prefDirec: List[Direction]): Option[CellId] =
    value.get(cellId) match {
      case None => None
      case Some(map) =>
        prefDirec.filter(map.contains(_)) match {
          case Nil => Some(map.values.head)
          case x :: xs => map.get(x)
        }
    }

  def addPath(cellId: CellId, newPath: (Direction, CellId)): WorldMap =
    this.copy(value = value + (cellId -> (value(cellId) + newPath)))

  def updateMap(oldCell: Cell, newCell: Cell): WorldMap = {
    assert(oldCell.cellId == newCell.cellId, "Cell IDs much match!")
    this.copy(cellIdToCellMap=cellIdToCellMap + ((newCell, newCell)))
  }

  def move(fromCell: CellId, direction: Direction): Option[CellId] =
    for {
      directions <- value.get(fromCell)
      cell <- directions.get(direction)
    } yield cell

  def directions(fromCell: CellId): List[String] =
    List((value.get(fromCell) match {
      case None => Nil
      case Some(map) => map.map { case (k, v) => s"${k}: ${v.shortDesc}" }
    }).mkString("\n"))
}

object WorldMap {
  implicit def cellToCellId(cell: Cell): CellId = cell.cellId

  def initialState: (WorldMap, CellId) =
    (WorldMap(Map((house, Map(North -> herbGarden, South -> street1)),
                  (street1, Map(North -> house, East -> street2)),
                  (street2, Map(West -> street1, East -> street3)),
                  (street3, Map(West -> street2, East -> street4)),
                  (street4, Map(West -> street3)),
                  (street5, Map(West -> street4)),
                  (street6, Map(West -> street5)),
                  (forest1, Map(North -> mountain1, West -> forest2,
                                South ->herbGarden)),
                  (mountain1, Map(North -> mine, South -> forest1)),
                  (mine, Map(South -> mountain1)),
                  (forest2, Map(East -> forest1, West -> forest3)),
                  (forest3, Map(East -> forest2, North -> forest4)),
                  (forest4, Map(South -> forest3)),
                  (forest5, Map(South -> forest4, North -> forestGrove)),
                  (forestGrove, Map(South -> forest5)),
                  (herbGarden, Map(South -> house, North -> forest1))),
              initialCellIdToCell),
     house)
  

  val house: Cell = Cell(
    CellId(),
    List(MorriganNote()),
    "a house",
    "You are in a house.",
    firstEnter = Some("You awake as a large black crow squawks and flies out of your window," +
    "\ndropping a note from its claws."))
  val street1: Cell = Cell(
    CellId(),
    List(),
    "a street",
    "You are on a street.  As a druid, you prefer more contact with nature.")
  val street2: Cell = Cell(
    CellId(),
    List(),
    "a street",
    "You are on a street.")
  val street3: Cell = Cell(
    CellId(),
    List(),
    "a street",
    "You are on a street.  There aren't many trees here.")
  val street4: Cell = Cell(
    CellId(),
    List(),
    "a street",
    "You are on a street.  This part of town looks kind of dodgy.")
  val street5: Cell = Cell(
    CellId(),
    List(),
    "a one way street",
    "Second one way street for Diana.")
  val street6: Cell = Cell(
    CellId(),
    List(),
    "a one way street",
    "One way street for Diana.")
  val herbGarden: Cell = Cell(
    CellId(),
    List(Planter(), Fragarach(isHidden=true)),
    "an herb garden",
    "You are in a lovely herb garden.",
    groundAccess=true)
  val forest1: Cell = Cell(
    CellId(),
    List(),
    "a forest path",
    "You are in the forest.  The trees are small here.",
    groundAccess=true)
  val mountain1: Cell = Cell(
    CellId(),
    List(),
    "a mountain path",
    "You are entering the mountains.  It's lovely but cool.",
    groundAccess=true)
  val mine: Cell = Cell(
    CellId(),
    List(),
    "a mine",
    "There is a mine ahead, but it looks too dangerous to enter.",
    groundAccess=true)
  val forest2: Cell = Cell(
    CellId(),
    List(),
    "a forest path",
    "You are in the forest.  A small stream wanders by.",
    groundAccess=true)
  val forest3: Cell = Cell(
    CellId(),
    List(),
    "a forest path",
    "The forest path begins to curve.  It smells of smoke.",
    groundAccess=true)
  val forest4: Cell = Cell(
    CellId(),
    List(),
    "a narrow forest path",
    "The trees become taller this deep in the forest.",
    groundAccess=true)
  val forest5: Cell = Cell(
    CellId(),
    List(),
    "deep forest",
    "You can sense that you're nearing a tethered tree.",
    groundAccess=true)
  val forestGrove: Cell = Cell(
    CellId(),
    List(TetheredTree()),
    "a grove",
    "Ah the grove finally, you should really put some tethered trees closer to home.",
    groundAccess=true)

  val initialCells: Set[Cell] = Set(house, street1, street2, street3, street4,
    street5, street6, herbGarden, mountain1, mine, forest1, forest2, forest3,
    forest4, forest5, forestGrove)
  val initialCellIdToCell: Map[CellId, Cell] =
    initialCells.map(cell => (cell.cellId, cell)).toMap
}
