package GameUtilities


import GameElement.{Grid, Position}

import scala.annotation.tailrec
import scala.io.StdIn

/**
  *
  */
object GameHandlerUtil {

  def gameModeChoice() : String =
  {
    println("Choose game mode (1,2,3 or 4)")
    StdIn.readLine()
  }

  /**
    *
    * @return
    */
  @tailrec
  def endGameInput(): Boolean = {
    println("Do you want to play again? (y for yes, n for no)")
    val choice = StdIn.readLine().toUpperCase
    choice match {
      case "Y" => true
      case "N" => false
      case _ => {
        println("Wrond input !!! please type Y for yes or N for no")
        endGameInput()
      }
    }
  }

  /**
    * Check with the initial position and the orientation of the ship if this one is still in the limits of the grid
    * @param p : the initial position
    * @param orientation : the orientation the player wants to place its ship
    * @param sizeShip : the size of the ship
    * @return : true if the position of the ship is still in the limits of the grid, else false
    */
  def positionLimitsCheck(p : Position, orientation: String,sizeShip : Int) : Boolean =
  {
    orientation match
    {
      case "HR" => p.y + (sizeShip - 1) <10
      case "HL" => p.y - (sizeShip - 1) > (-1)
      case "VU" => p.x - (sizeShip - 1) > (-1)
      case "VD" => p.x + (sizeShip - 1) <10
    }
  }

  /**
    * Generate the list of positions of a ship starting with a position the the user typed and according to an orientation
    * @param orientation : the orientation of the ship :
    *                    HR : Horizontal Right
    *                    HL : Horizontal Left
    *                    VU : Vertical Up
    *                    VD : Vertical Down
    * @param shipSize : The size of the ship to be place
    * @param p : the position that the player choose
    * @return : a list of Position which the first element is the one that the player seized
    */
  def generateShipPosition(orientation: String, shipSize: Int, p: Position): List[Position] = {
    if (shipSize == 0)
      Nil
    else {
      orientation match {
        case "HR" => p :: generateShipPosition(orientation, shipSize - 1, Position(p.x, p.y + 1))
        case "HL" => p :: generateShipPosition(orientation, shipSize - 1, Position(p.x, p.y - 1))
        case "VU" => p :: generateShipPosition(orientation, shipSize - 1, Position(p.x - 1, p.y))
        case "VD" => p :: generateShipPosition(orientation, shipSize - 1, Position(p.x + 1, p.y))
      }
    }
  }

  def aiLevelInput() : Int =
  {
    println("Choose level mode")
    val choice = StdIn.readLine()
    choice match
    {
      case "1" => 1
      case "2" => 2
      case "3" => 3
      case _ => {
        println("wrong choice !!! choose 1,2 or 3")
        aiLevelInput()
      }
    }
  }

  def nextTurn(currentTurn: String) : String =
    {
      currentTurn match
      {
        case "player1"  => "player2"
        case "player2"  => "player1"
        case "player"   => "ai"
        case "ai"       => "player"
      }
    }

  def middleAiGetNextPosition(currentTarget: Position,orientation : String,grid : Grid):List[Position] =
  {
    if(orientation == "V")
      {
        val p1 = Position(currentTarget.x+1,currentTarget.y)
        val p2 = Position(currentTarget.x-1,currentTarget.y)
        if((p1.x <10 && p1.x>(-1) ) && (p2.x <10 && p2.x>(-1)) && (grid.grid(p1.x)(p1.y)<1 ) && (grid.grid(p2.x)(p2.y)<1 ) )
          {
            List.apply(p1,p2)
          }
        else if((p1.x <10 && p1.x>(-1) ) && (grid.grid(p1.x)(p1.y)<1 ))
          {
            List.apply(p1)
          }
        else if( (p2.x <10 && p2.x>(-1)) && (grid.grid(p2.x)(p2.y)<1 ))
          {
            List.apply(p2)
          }
        else
        {
          List.empty[Position]
        }
      }
    else
      {
        val p1 = Position(currentTarget.x,currentTarget.y+1)
        val p2 = Position(currentTarget.x,currentTarget.y-1)
        if((p1.y <10 && p1.y>(-1) ) && (p2.y <10 && p2.y>(-1)) && (grid.grid(p1.x)(p1.y)<1 ) && (grid.grid(p2.x)(p2.y)<1 ) )
        {
          List.apply(p1,p2)
        }
        else if((p1.y <10 && p1.y>(-1) ) && (grid.grid(p1.x)(p1.y)<1 ))
        {
          List.apply(p1)
        }
        else if( (p2.y <10 && p2.y>(-1)) && (grid.grid(p2.x)(p2.y)<1 ))
        {
          List.apply(p2)
        }
        else
        {
          List.empty[Position]
        }
      }
  }
}
