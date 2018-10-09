package GameUtilities

import GameElement.{Grid, Player, Position}

import scala.io.StdIn
import scala.util.Random

/**
  * his class contains the functions that represent the action of the medium Ai
  */
object MediumAiActions {

  /**
    * Get the next potential targets for the medium Ai
    * @param currentTarget : the potential targets that are already stored
    * @param orientation : the orientation of the positions to target
    * @param grid : the enemy's grid
    * @return : a list of positions to target for the medium AI
    */
  def mediumAiGetNextTargets(currentTarget: Position,orientation : String,grid : Grid): List[Position] =
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

  /**
    * Get the next positions that the Ai will aim at its next turn
    * @param targets : the potential next targets
    * @param gameState : contains the information of the players
    * @param r : the random variable that the ai will use
    * @return : the next position to shot at
    */
  def getNextPositionShot(targets:(List[Position],String),gameState : GameState, r : Random) : Position =
  {
    if(targets._1.isEmpty)
      GeneralActionsForAi.aiMakeAShot(gameState.currentPlayer,r)
    else
    {
      targets._1.head
    }
  }

  /**
    * delete the position of the last shot
    * @param targets : the potential targets
    * @return : a tuple of list of positions with the orientation
    */
  def deleteUsedShot(targets:(List[Position],String)) : (List[Position],String) =
  {
    if(targets._1.isEmpty)
      targets
    else
      (targets._1.tail,targets._2)
  }

  /**
    * get the orientation of the next targets
    * @param nextTargets : the potential targets that are already stored
    * @param r : the random variable that the ai will use
    * @return : the orientation of the next positions to target
    */
  def getOrientation(nextTargets :(List[Position],String),r:Random) : String =
  {
    if(nextTargets._1.nonEmpty)
      nextTargets._2
    else
    {
      val randomOrientation = r.nextInt(2)
      if(randomOrientation == 0)
        "H"
      else
        "V"
    }
  }

  /**
    * The behaviour of the Ai when it is its turn
    * @param gameState : ontains the information of the players
    * @param r : the random variable that the ai will use
    * @param nextTargets : the potential next targets
    * @return : a tuple of the updated game state and the list of the potential targets with its orientation
    */
  def mediumAiTurn(gameState : GameState,r : Random,nextTargets :(List[Position],String)) : (GameState,(List[Position],String)) =
  {
    val positionShot = getNextPositionShot(nextTargets,gameState,r)
    if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == -1)
      {
        val newCurrentPlayer = Player.missShot(gameState.currentPlayer,positionShot)

        val newNextPlayer = Player.receiveMissShot(gameState.nextPlayer,positionShot)

        val newGameState = GameState(newNextPlayer,newCurrentPlayer)
        val newNextTargets = deleteUsedShot(nextTargets)
        (newGameState,newNextTargets)
      }
    else
      {
        val newCurrentPlayer = Player.successShot(gameState.currentPlayer,positionShot)
        val newNextPlayer = Player.receiveSuccessShot(gameState.nextPlayer,positionShot)
        val newGameState = GameState(newNextPlayer,newCurrentPlayer)
        if(!Player.checkShipState(newNextPlayer,positionShot))
          {
            if (gameState.currentPlayer.playerType == "ai")
            {
              println("Ship Destroyed!!!!!")
              StdIn.readLine("Press any key to continue")
            }
            val newNextTarget = (List.empty[Position],"")
            (newGameState,newNextTarget)
          }
        else
          {
            val orientation = getOrientation(nextTargets,r)
            val updatedNextTarget = deleteUsedShot(nextTargets)
            val nextPositionsToTarget = mediumAiGetNextTargets(positionShot,orientation,newCurrentPlayer.enemyGrid)
            val newNextTargets = (updatedNextTarget._1 ++ nextPositionsToTarget,orientation)
            (newGameState,newNextTargets)
          }
      }
  }
}
