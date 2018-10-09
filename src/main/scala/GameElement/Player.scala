package GameElement

import scala.annotation.tailrec

/**
  * this class represents a player.
  * It will place the ships of the player on his grid, update the grids during the game, check the state of his ships
  * A player can be a user (human) or can be an Ai
  * @param fleet : the fleet of the player coposed of his ships and its position on the grid
  * @param ownGrid : the grid of the player where he will put his ships and the result of the enemy shots
  * @param enemyGrid : the enemy grid where player will put the results of his shots
  * @param playerType : the type of the player : a player or ai, an ai can be an easy, medium or hard ai
  */
case class Player(fleet : List[(List[Position],Ship)], ownGrid : Grid, enemyGrid : Grid,playerType : String)

object Player
{
  /**
    *This function will add the ship of the player on the grid
    * @param player : The player that will place the ships
    * @param fleetElement : the fleet of the player composed by the ships and their position
    * @return : player with his greed containing his ships (the element of the listing containing a square of the
    *         ship is equal to 0)
    */
  def addShip(player : Player, fleetElement : (List[Position],Ship)): Player =
  {
    @tailrec
    def addShipTailRec(player : Player, fleetElement : (List[Position],Ship),index : Int ): Player =
    {
      if(index == fleetElement._1.size)
        {
          player.copy( player.fleet :+ fleetElement)
        }
      else
        {
          val newPlayer = player.copy(ownGrid = Grid.updateGrid(player.ownGrid,fleetElement._1(index),0))
          addShipTailRec(newPlayer,fleetElement,index+1)
        }
    }

    addShipTailRec(player,fleetElement,0)
  }



  /**
    *check if the square in the position passed in parameters are already occupied or not
    * @param player : the player whose grid nthat we are testing, belongs to
    * @param positions : the positions we want to check
    * @return : true if one of the position is occupied and false if none of them are occupied
    */
  def occupiedPosition(player : Player,positions : List[Position]): Boolean =
  {
    if(positions.isEmpty)
      {
        false
      }

    else
      {
        if(player.ownGrid.grid(positions.head.x)(positions.head.y) == 0)
          true
        else
          occupiedPosition(player,positions.tail)
      }

  }

  /**
    *This function updates the gird of the player who received the missed shot,
    * @param player : the player who received the shot
    * @param shotPosition : the position where the shot landed
    * @return : a player with his grid updated
    */
  def receiveMissShot(player : Player, shotPosition : Position) : Player = {
      val newOwnGrid = Grid.updateGrid(player.ownGrid, shotPosition, 1)
      player.copy(ownGrid = newOwnGrid)
  }

  /**
    *This function updates the gird of the player who received the missed shot
    * @param player : the player who received the shot
    * @param shotPosition : the position where the shot landed
    * @return : a player with his grid updated
    */
  def receiveSuccessShot(player : Player, shotPosition : Position) : Player =
  {
    val oldShip = player.fleet.filter(x => x._1.contains(shotPosition))(0)
    val updatedShip = Ship.shipDamaged(oldShip._2)
    val updatedFleetElement = {
      if (updatedShip.squareNumber == 0)
        (oldShip._1, updatedShip.copy(operational = false))
      else
        (oldShip._1, updatedShip)
    }
    val indexOfElementToUpdate = player.fleet.indexOf(oldShip)
    val newOwnGrid = Grid.updateGrid(player.ownGrid,shotPosition,2)

    player.copy(fleet = player.fleet.updated(indexOfElementToUpdate,updatedFleetElement),ownGrid = newOwnGrid)
  }

  /**
    * this function update the grid of the player's enemy, the one where he records his shots.
    * It is called when the player make a miss shot
    * @param player : the player whio makes the shot
    * @param shotPosition : the position of the shot
    * @return : player with his enemy grid updated
    */
  def missShot(player : Player, shotPosition : Position) : Player =
  {
    val newEnemyGrid = Grid.updateGrid(player.enemyGrid, shotPosition, 1)
    player.copy(enemyGrid = newEnemyGrid)
  }

  /**
    * this function update the grid of the player's enemy, the one where he records his shots.
    * It is called when the player make a success shot
    * @param player : the player whio makes the shot
    * @param shotPosition : the position of the shot
    * @return : player with his enemy grid updated
    */
  def successShot(player : Player, shotPosition : Position) : Player =
  {
    val newEnemyGrid = Grid.updateGrid(player.enemyGrid, shotPosition, 2)
    player.copy(enemyGrid = newEnemyGrid)
  }

  /**
    * this function checks if a ship has been destroyed or not every time one of his squares got touched
    * @param player : the player whose ship belongs to
    * @param shotPosition : the position of the ship sqaure that has been damaged
    * @return : boolean true if its not destroyed and false if it is
    */
  def checkShipState(player : Player, shotPosition : Position) : Boolean =
  {
    val shipToCheck = player.fleet.filter(x => x._1.contains(shotPosition))(0)
    shipToCheck._2.operational
  }

  /**
    * Check if the fleet is destroyed or not
    * @param fleet : the fleet we want to check
    * @return : false if the fleet is destroyed, else return true
    */
    @tailrec
    def checkFleetState(fleet: List[(List[Position],Ship)]) : Boolean =
    {
      if(fleet.size>0)
       fleet.head._2.operational || checkFleetState(fleet.tail)
      else
        false
    }


}
