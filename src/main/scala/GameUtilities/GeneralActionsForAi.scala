package GameUtilities

import GameElement.{Grid, Player, Position, Ship}

import scala.annotation.tailrec
import scala.util.Random

/**
  *
  */
object GeneralActionsForAi {

  /**
    *
    * @param shipSize
    * @param player
    * @param r
    * @return
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
    *
    * @param shipsClass
    * @param r
    * @param aiType
    * @return
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
    *
    * @param r
    * @return
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
