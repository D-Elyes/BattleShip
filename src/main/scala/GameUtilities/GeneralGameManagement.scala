package GameUtilities


import GameElement.{Position}
import scala.annotation.tailrec
import scala.io.StdIn

/**
  * This class will handle the inputs of the user for the mode choices (player vs player or player vs ai, etc...)
  * It also contains functions that will be used by both player as a user and player as a ai
  */
object GeneralGameManagement {

  /**
    * Will ask the user to choose the game mode
    * @return : the choice of the user for the game mode
    */
  def gameModeChoice() : String =
  {
    println("Choose game mode (1,2,3 or 4)")
    StdIn.readLine()
  }

  /**
    *When a game is over, the user can choose to play again in the same mode or quit to the main menu
    * this function will ask the user to make a choice, play again or quit
    * @return : the choice of the user
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
    * this function will be used for both player as a user and player as an ai
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
    * this function will be used for both player as a user and player as an ai
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

  /**
    * This function will ask the user to choose the ai level which he will play against
    * @return : the choice of user
    */
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
}
