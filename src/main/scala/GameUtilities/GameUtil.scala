package GameUtilities


import GameElement.Position

import scala.annotation.tailrec
import scala.io.StdIn

object GameUtil {

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
}
