package GameElement

/**
  * This is the reperesentation of the ship that will be ised on the game
  *
  * @param operational : this attribute shows if the ship is still operational or not, it means if it is destroyed or not
  *                    true if it is still operational (not destroyed) and false if it is not (destroyedà
  *                    at the begginning of a game this attribute will be at true, and when all its squares are touched, it changes to false
  * @param squareNumber : the number of square that a ship occupies. At the beginning of a game the number of squares are equal
  *                     to the size of the ship and if a square it touched by the enemy the number decrease, if it reaches 0
  *                     the ship is considered as destroyed
  */
case class Ship(operational : Boolean, squareNumber : Int)

object Ship
{

  /**
    *This function is called when the ship is damaged
    * @param ship : the ship that has been damaged
    * @return : return a new ship which is the copy of the one that has been damaged with less square number
    */
  def shipDamaged(ship : Ship) = ship.copy(squareNumber = ship.squareNumber-1)

  /**
    *This function is called when the ship is destroyed
    * @param ship : the ship that has been destroyed
    * @return : return the copy of the destroyed ship with the attribute operational at false
    */
  def shipDestroyed(ship : Ship)=ship.copy(operational = false)
}