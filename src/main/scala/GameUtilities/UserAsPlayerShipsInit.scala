package GameUtilities

import GameElement.{Grid, Player, Position, Ship}
import GameInterface.Render

import scala.annotation.tailrec

/**
  *This class will handle the initialisation of the ship for a user
  * it will generate all the ships and place them on the grid.
  */
object UserAsPlayerShipsInit {


  /**
    * This function will generate the ship with its corresponding positions
    * it will return the ship with its positions which will be added to the user's grid
    * @param shipSize : the shize of the ship to generate
    * @param player : the player who places the ship
    * @return : a tuple of list of positions and the ship
    */
  @tailrec
  def generateShipWithPosition(shipSize : Int, player : Player) : (List[Position],Ship) =
  {
    val ship = Ship(true,shipSize)
    val line = PlayerAsUserInputs.seizeLineNumberShip()
    val column = PlayerAsUserInputs.seizeColumnNumberShip()
    val orientation = PlayerAsUserInputs.seizeOrientationShip()
    if(GeneralGameManagement.positionLimitsCheck(Position(line,column),orientation,shipSize))
      {
        val positions = GeneralGameManagement.generateShipPosition(orientation,shipSize,Position(line,column))
        if(!Player.occupiedPosition(player,positions))
        {
          (positions,ship)
        }
        else
        {
          println("Ships overlapping !!!!Choose another position for this ship")
          generateShipWithPosition(shipSize,player)
        }
      }
    else
      {
        println("Ship position out of limits !!!! please enter another position")
        generateShipWithPosition(shipSize,player)
      }

  }

  /**
    * generate all player's ships with their respective positions
    * update the player's grid
    * @param shipsClass : the list of ships that can be added to a game
    * @return : player with all of his ships and his grid updated
    */
  def generatePlayerWithItsShip(shipsClass : List[(String,Int)],playerType : String) : Player =
  {

    @tailrec
    def generatePlayerWithItsShipTailRec(player : Player, index : Int) : Player =
    {
      if(index == shipsClass.size)
        player
      else
        {
          println("Positioning of : "+shipsClass(index)._1 +" ("+shipsClass(index)._2+" squares)")
          val shipSize = shipsClass(index)._2
          val shipWithPositions = generateShipWithPosition(shipSize,player)
          val playerWithShipAdded = Player.addShip(player,shipWithPositions)
          Render.playerGridRenderer(playerWithShipAdded.ownGrid)
          generatePlayerWithItsShipTailRec(playerWithShipAdded,index+1)
        }
    }
    val player = Player(List.empty[(List[Position],Ship)],Grid(),Grid(),playerType)
    Render.playerGridRenderer(player.ownGrid)
    generatePlayerWithItsShipTailRec(player,0)
  }


}