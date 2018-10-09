package GameUtilities

import GameElement.{Grid, Player, Position}

import scala.io.StdIn
import scala.util.Random

/**
  *
  */
object MediumAiActions {

  /**
    *
    * @param currentTarget
    * @param orientation
    * @param grid
    * @return
    */
  def mediumAiGetNextTargets(currentTarget: Position,orientation : String,grid : Grid):List[Position] =
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
    *
    * @param nextTargets
    * @param gameState
    * @param r
    * @return
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
    *
    * @param targets
    * @return
    */
  def deleteUsedShot(targets:(List[Position],String)) : (List[Position],String) =
  {
    if(targets._1.isEmpty)
      targets
    else
      (targets._1.tail,targets._2)
  }

  /**
    *
    * @param nextTargets
    * @param r
    * @return
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
    *
    * @param gameState
    * @param r
    * @param nextTargets
    * @return
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
