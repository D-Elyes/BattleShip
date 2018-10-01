package GameElement

import scala.annotation.tailrec

/**
  *
  * @param fleet
  * @param ownGrid
  * @param enemyGrid
  */
case class Player(fleet : List[(List[Position],Ship)], ownGrid : Grid, enemyGrid : Grid)

object Player
{
  /**
    *
    * @param player
    * @param fleetElement
    * @return
    */
  def addShip(player : Player, fleetElement : (List[Position],Ship)): Player =
  {
    @tailrec
    def addShipTailRec(player : Player, fleetElement : (List[Position],Ship),index : Int ): Player =
    {
      if(index == fleetElement._1.size)
        {
          player.copy(fleetElement :: player.fleet)
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
    *
    * @param player
    * @param positions
    * @return
    */
  def occupiedPosition(player : Player,positions : List[Position]): Boolean =
  {
    if(positions.size == 0)
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
    *
    * @param player
    * @param shotPosition
    * @return
    */
  def receiveMissShot(player : Player, shotPosition : Position) : Player = {
      val newOwnGrid = Grid.updateGrid(player.ownGrid, shotPosition, 1)
      player.copy(ownGrid = newOwnGrid)
  }

  /**
    *
    * @param player
    * @param shotPosition
    * @return
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
    *
    * @param player
    * @param shotPosition
    * @return
    */
  def missShot(player : Player, shotPosition : Position) : Player =
  {
    val newEnemyGrid = Grid.updateGrid(player.enemyGrid, shotPosition, 1)
    player.copy(ownGrid = newEnemyGrid)
  }

  /**
    *
    * @param player
    * @param shotPosition
    * @return
    */
  def successShot(player : Player, shotPosition : Position) : Player =
  {
    val newEnemyGrid = Grid.updateGrid(player.enemyGrid, shotPosition, 2)
    player.copy(ownGrid = newEnemyGrid)
  }

  /**
    *
    * @param player
    * @param shotPosition
    * @return
    */
  def checkShipState(player : Player, shotPosition : Position) : Boolean =
  {
    val shipToCheck = player.fleet.filter(x => x._1.contains(shotPosition))(0)
    shipToCheck._2.operational
  }
}
