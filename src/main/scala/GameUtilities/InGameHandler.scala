package GameUtilities

import GameElement.{Player, Position, Ship}


trait InGameHandler {

  def generateShipPosition(orientation: String, shipSize: Int, p: Position): List[Position]
  def generateShipWithPosition(shipSize : Int, player : Player) : (List[Position],Ship)
  def generatePlayerWithItsShip(shipsClass : List[(String,Int)]) : Player
  def makeAShot(player : Player):Position

}
