package GameUtilities

import GameElement.{Grid, Player, Position, Ship}

import scala.annotation.tailrec
import scala.util.Random

object AiInGameHandler {

  @tailrec
  def generateShipWithPosition(shipSize : Int, player : Player, r : Random) : (List[Position],Ship) =
  {
    val ship = Ship(true,shipSize)
    val line = r.nextInt(10)
    val column = r.nextInt(10)
    val orientation = seizeOrientationShip(r)
    if(GameHandlerUtil.positionLimitsCheck(Position(line,column),orientation,shipSize))
      {
        val positions = GameHandlerUtil.generateShipPosition(orientation,shipSize,Position(line,column))
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

  def generatePlayerWithItsShip(shipsClass : List[(String,Int)],r:Random) : Player =
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
        val shipWithPositions = generateShipWithPosition(shipSize,player,r)
        val playerWithShipAdded = Player.addShip(player,shipWithPositions)
        generatePlayerWithItsShipTailRec(playerWithShipAdded,index+1)
      }
    }
    val player = Player(List.empty[(List[Position],Ship)],Grid(),Grid())
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
