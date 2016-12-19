package adventure

import scala.util.parsing.combinator._

sealed trait Command

object Command extends  {
  private object parser extends RegexParsers {
    lazy val goOpt: Parser[Option[String]] = "go".?

    lazy val quit: Parser[Command] = ("quit" | "q") ^^^ Quit

    lazy val pickup: Parser[String] = "pick up" | "get" | "retrieve" | "take"
    lazy val drop: Parser[String] = "drop" | "set down"
    lazy val fragarach: Parser[String] = ("sword" | "fragarach")
    lazy val iron: Parser[Option[String]] = "iron".?
    lazy val amulet: Parser[String] = iron ~> "amulet"
    lazy val note: Parser[String] = "note"
    lazy val planter: Parser[String] = "planter"
    type II = Inspectable[Item]
    type IC = Inspectable[Creature]
    lazy val getItem: Parser[Command] =
      pickup ~ fragarach ^^^ Get(Right(Fragarach(false))) |
      pickup ~ amulet ^^^ Get(Right(IronAmulet())) |
      pickup ~ ".*".r ^^ { case a ~ b => Get(Left(b)) }
    lazy val dropItem: Parser[Command] =
      drop ~ fragarach ^^^ Drop(Right(Fragarach(false))) |
      drop ~ amulet   ^^^ Drop(Right(IronAmulet())) |
      drop ~ ".*".r ^^ { case a ~ b => Drop(Left(b)) }
    lazy val inspectItem: Parser[Command] =
      inspect ~ note ^^^ Inspect(Right[String, II](MorriganNote())) |
      inspect ~ planter ^^^ Inspect(Right[String, II](Planter()))   |
      inspect ~ fragarach ^^^ Inspect(Right[String, II](Fragarach(false))) |
      inspect ~ "loki" ^^^ Inspect(Right[String, IC](Loki(WorldMap.house))) |
      inspect ~ "diana" ^^^ Inspect(Right[String, IC](Diana(WorldMap.house))) |
      inspect ~ "tree" ^^^ Inspect(Right[String, II](TetheredTree())) |
      inspect ~ amulet ^^^ Inspect(Right[String, II](IronAmulet())) |
      inspect ~ ".*".r ^^ { case a ~ b => Inspect(Left(b)) }
    lazy val equipItem: Parser[Command] =
      equip ~ fragarach ^^^ Equip(Right(Fragarach(false))) |
      equip ~ amulet ^^^ Equip(Right(IronAmulet())) |
      equip ~ ".*".r ^^ { case a ~ b => Equip(Left(b)) }
    lazy val equip: Parser[String] = "equip" | "eq"
    lazy val inspect: Parser[String] = "inspect" | "look"
    lazy val north: Parser[Command] = goOpt ~ "north" ^^^ Go(North)
    lazy val east: Parser[Command] = goOpt ~ "east" ^^^ Go(East)
    lazy val south: Parser[Command] = goOpt ~ "south" ^^^ Go(South)
    lazy val west: Parser[Command] = goOpt ~ "west" ^^^ Go(West)
    lazy val view: Parser[Option[String]] = "view".?
    lazy val inventory: Parser[Command] =
      view ~ ("inventory" | "inv") ^^^ Inventory
    lazy val attack: Parser[String] = "attack" | "kill"
    lazy val attackCreature: Parser[Command] =
      attack ~ "loki" ^^^ Attack(Right(Loki(WorldMap.house))) |
      attack ~ "diana" ^^^ Attack(Right(Diana(WorldMap.house))) |
      attack ~ ".*".r ^^ { case a ~ b => Attack(Left(b)) }
    lazy val help: Parser[Command] = ("help" | "?" | "man") ^^^ Help
    lazy val cast: Parser[String] = "cast"
    lazy val castSpell: Parser[Command] =
      cast ~ "camo" ^^^ Cast(Right(Camo)) |
      cast ~ "portal" ^^^ Cast(Right(Portal)) |
      cast ~ ".*".r ^^ { case a ~ b => Cast(Left(b)) }

    def grammar: Parser[Command] = quit | north | east | south | west |
      getItem | inspectItem | dropItem | inventory | equipItem |
      attackCreature | help | castSpell
  }

  import parser._

  def parse(line: String): Either[String, Command] =
    parseAll(grammar, line.toLowerCase) match {
      case Success(x, _) => Right(x)
      case failure : NoSuccess => Left("Sorry, I don't understand.\n")
  }
}

case class Cast(spell: Either[String, Spell]) extends Command
case class Attack(creature: Either[String, Creature]) extends Command
case class Inspect[T](item: Either[String, Inspectable[T]]) extends Command
case class Get(item: Either[String, Inventoryable]) extends Command
case class Drop(item: Either[String, Inventoryable]) extends Command
case class Equip(item: Either[String, Equipable]) extends Command
case class Go(direction: Direction) extends Command
case object Quit extends Command
case object Inventory extends Command
case object Help extends Command
