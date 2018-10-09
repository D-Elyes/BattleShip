package GameUtilities

import GameElement.{Grid, Player, Position, Ship}
import scala.annotation.tailrec
import scala.util.Random

/**
  * This class contains the functions that can be used for all the Ais
  */
object GeneralActionsForAi {

  /**
    * Generate a ship with its corresponding position
    * @param shipSize : the size of the ship
    * @param player : the player that will ad the ship
    * @param r ; the random variable that the Ai will use
    * @return : a tuple containing the ship with its corresponding positions
    */
  @tailrec
  def generateShipWithPosition(shipSize : Int, player : Player, r : Random) : (List[Position],Ship) =
  {
    val ship = Ship(true,shipSize)
    val line = r.nextInt(10)
    val column = r.nextInt(10)
    val orientation = seizeOrientationShip(r)
    if(GeneralGameManagement.positionLimitsCheck(Position(line,column),orientation,shipSize))
      {
        val positions = GeneralGameManagement.generateShipPosition(orientation,shipSize,Position(line,column))
        if(!Player.occupiedPosition(player,positions))
        {
          (positions,ship)
        }
        else
        {
          generateShipWithPosition(shipSize,player,r)
        }
      }
    else
      {
        generateShipWithPosition(shipSize,player,r)
      }
  }

  /**
    * Generate a player with its position place on his grid
    * @param shipsClass: the  list of ship that will be place
    * @param r : the random variable that the Ai will use
    * @param aiType : the type of the Ai
    * @return : it return a player with its corresponding informations : his ships, his grids, his tupe
    */
  def generatePlayerWithItsShip(shipsClass : List[(String,Int)],r:Random, aiType:String) : Player =
  {
    @tailrec
    def generatePlayerWithItsShipTailRec(player : Player, index : Int) : Player =
    {
      if(index == shipsClass.size)
        player
      else
      {
        val shipSize = shipsClass(index)._2
        val shipWithPositions = generateShipWithPosition(shipSize,player,r)
        val playerWithShipAdded = Player.addShip(player,shipWithPositions)
        generatePlayerWithItsShipTailRec(playerWithShipAdded,index+1)
      }
    }
    val player = Player(List.empty[(List[Position],Ship)],Grid(),Grid(),aiType)
    generatePlayerWithItsShipTailRec(player,0)
  }

  /**
    * the shot action of an Ai
    * @param player : the player (Ai) that will make the shot
    * @param r : the random variable that the Ai will use
    * @return : The position of the shot
    */
  def aiMakeAShot(player : Player,r:Random):Position =
  {
    val line= r.nextInt(10)
    val column = r.nextInt(10)
    val position = Position(line,column)
    if(player.enemyGrid.grid(position.x)(position.y) == 1 || player.enemyGrid.grid(position.x)(position.y) == 2)
    {
      aiMakeAShot(player,r)
    }
    else
    {
      position
    }
  }

  /**
    * Seize the orientation of a ship to be place on the gird
    * @param r : the random varaible that the Ai will use
    * @return : A string representing the orientation of the ship
    */
  def seizeOrientationShip(r: Random) : String =
  {
    val orientation = r.nextInt(4)
    orientation match {
      case 0 => "HR"
      case 1 => "HL"
      case 2 => "VU"
      case 3 => "VD"
    }
  }
}
