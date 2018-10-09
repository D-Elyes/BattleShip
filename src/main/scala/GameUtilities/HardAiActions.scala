package GameUtilities

import GameElement.{Grid, Player, Position}

import scala.io.StdIn
import scala.util.Random

/**
  *
  */
object HardAiActions {

  /**
    *
    * @param nextTargets
    * @return
    */
  def hardAiGetNextTarget(nextTargets : ((List[Position],String),(List[Position],String))): Position =
  {
    if (nextTargets._1._1.nonEmpty)
    {
      nextTargets._1._1.head
    }
    else
    {

      nextTargets._2._1.head
    }

  }
  /**
    *
    * @param nextTargets
    * @param position
    * @return
    */
  def deleteUsedTargetByHardAi(nextTargets : ((List[Position],String),(List[Position],String)),position : Position) : ((List[Position],String),(List[Position],String)) =
  {
    if(nextTargets._1._1.contains(position))
    {
      ((nextTargets._1._1.tail,nextTargets._1._2),nextTargets._2)
    }
    else if(nextTargets._2._1.contains(position))
    {
      (nextTargets._1,(nextTargets._2._1.tail,nextTargets._2._2))
    }
    else
    {
      nextTargets.copy()
    }
  }

  /**
    *
    * @param targets
    * @param position
    * @param grid
    * @return
    */
  def getNextPositionToTargetForHardAi(targets : ((List[Position],String),(List[Position],String)),position : Position, grid : Grid) : ((List[Position],String),(List[Position],String))=
  {
    if(targets._1._1.contains(position))
    {
      val positions = MediumAiActions.mediumAiGetNextTargets(position,targets._1._2,grid)
      val nextPositions = (positions ++targets._1._1.tail ,targets._1._2)
      (nextPositions,targets._2)
    }
    else if(targets._2._1.contains(position))
    {
      val positions = MediumAiActions.mediumAiGetNextTargets(position,targets._2._2,grid)
      val nextPositions = (positions ++ targets._2._1.tail  ,targets._2._2)
      (targets._1,nextPositions)
    }
    else
    {
      val horizontalTarget = MediumAiActions.mediumAiGetNextTargets(position,"H",grid)
      val verticalTarget = MediumAiActions.mediumAiGetNextTargets(position,"V",grid)
      ((horizontalTarget,"H"),(verticalTarget,"V"))
    }
  }

  /**
    *
    * @param gameState
    * @param nextTarger
    * @param r
    * @return
    */
  def getNextShoPosition(gameState: GameState,nextTarger : ((List[Position],String),(List[Position],String)),r:Random) : Position =
  {
    if(nextTarger._1._1.isEmpty && nextTarger._2._1.isEmpty)
      GeneralActionsForAi.aiMakeAShot(gameState.currentPlayer,r)
    else
    {
      HardAiActions.hardAiGetNextTarget(nextTarger)
    }
  }

  /**
    *
    * @param checkHorizontaol
    * @param nextTargets
    * @return
    */
  def updateCheckHorizontal(checkHorizontaol : Boolean,nextTargets : ((List[Position],String),(List[Position],String))) : Boolean =
  {
    if(nextTargets._2._1.isEmpty)
      false
    else
      checkHorizontaol
  }

  def aiHardTurn(r:Random,gameState : GameState,nextTarget : ((List[Position],String),(List[Position],String)),
                  checkHorizontal : Boolean) : (GameState, (((List[Position],String),(List[Position],String)),Boolean)) =
  {
    val positionShot = getNextShoPosition(gameState,nextTarget,r)
    if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == -1)
      {
        val newCurrentPlayer = Player.missShot(gameState.currentPlayer,positionShot)
        val newNextPlayer = Player.receiveMissShot(gameState.nextPlayer,positionShot)
        val newGameState = GameState(newNextPlayer,newCurrentPlayer)
        val newCheckHorizontal = updateCheckHorizontal(checkHorizontal,nextTarget)
        val newNextTarget = HardAiActions.deleteUsedTargetByHardAi(nextTarget,positionShot)
        (newGameState,(newNextTarget,newCheckHorizontal))
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

            val newNextTarget = deleteUsedTargetByHardAi(nextTarget,positionShot)
            val checkHorizontalUpdate = newNextTarget._2._1.nonEmpty
            (newGameState,(newNextTarget,checkHorizontalUpdate))
          }
        else
          {
            if(nextTarget._1._1.isEmpty && nextTarget._2._1.nonEmpty && checkHorizontal)
            {
              val horizontalPositions = MediumAiActions.mediumAiGetNextTargets(positionShot,"H",newCurrentPlayer.enemyGrid)
              val checkHorizontalUpdate = false
              val verticalTarget = HardAiActions.getNextPositionToTargetForHardAi(nextTarget,positionShot,newCurrentPlayer.enemyGrid)
              val newNextTarget = ((horizontalPositions,"H"),verticalTarget._2)
              (newGameState,(newNextTarget,checkHorizontalUpdate))
            }
            else
            {
              val newNextTarget = HardAiActions.getNextPositionToTargetForHardAi(nextTarget,positionShot,newCurrentPlayer.enemyGrid)
              (newGameState,(newNextTarget,checkHorizontal))
            }
          }
      }
  }
}
