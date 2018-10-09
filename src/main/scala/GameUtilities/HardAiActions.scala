package GameUtilities

import GameElement.{Grid, Player, Position}

import scala.io.StdIn
import scala.util.Random

/**
  * This class contains the functions that represent the action of the hard Ai
  */
object HardAiActions {

  /**
    * Get the next target position that the Ai will aim
    * @param nextTargets : the potential targets the the Ai will aim
    * @return : the position that the Ai will aim
    */
  def hardAiGetNextTargets(nextTargets : ((List[Position],String),(List[Position],String))): Position =
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
    * Delete the position of the target that the Ai has just aimed
    * @param nextTargets : The potential next targets
    * @param position : the position that the Ai has just aimed
    * @return : the potential next targets updated
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
    * got the potential targets for the next turns
    * @param targets : the potential targets that are already stored
    * @param position : the position of the last successful shot
    * @param grid : the enemy's grid
    * @return : a tuple containing the potential targets at the horizontal and at the vertical
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
    * Get the next position for the Ai to aim
    * @param gameState : contains the information of the players
    * @param nextTarger : the potential next targets, can be empty
    * @param r : the random variable that the ai will use
    * @return : a position that the Ai will aim for its next turn
    */
  def getNextShotPosition(gameState: GameState,nextTarger : ((List[Position],String),(List[Position],String)),r:Random) : Position =
  {
    if(nextTarger._1._1.isEmpty && nextTarger._2._1.isEmpty)
      GeneralActionsForAi.aiMakeAShot(gameState.currentPlayer,r)
    else
    {
      HardAiActions.hardAiGetNextTargets(nextTarger)
    }
  }

  /**
    * Update if the Ai should check for the horizontal position or not
    * @param checkHorizontaol : says if the Ai should check for the horizontal positions or not
    * @param nextTargets : the potential next targets
    * @return : return true if the Ai should check, else false
    */
  def updateCheckHorizontal(checkHorizontaol : Boolean,nextTargets : ((List[Position],String),(List[Position],String))) : Boolean =
  {
    if(nextTargets._2._1.isEmpty)
      false
    else
      checkHorizontaol
  }

  /**
    * The behaviour of the Ai when it is its turn
    * @param r :the random variable that the ai will use
    * @param gameState : ontains the information of the players
    * @param nextTarget : the potential next targets
    * @param checkHorizontal : says if the Ai should check for the horizontal positions or not
    * @return : a tuple of the updated game state depending on the action of the Ai and the potential targets with the information
    *         if the Ai should check for horizontal positions or not
    */
  def aiHardTurn(r:Random,gameState : GameState,nextTarget : ((List[Position],String),(List[Position],String)),
                  checkHorizontal : Boolean) : (GameState, (((List[Position],String),(List[Position],String)),Boolean)) =
  {
    val positionShot = getNextShotPosition(gameState,nextTarget,r)
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
