package GameUtilities

import Game.BattleShip.{Game, GameState}
import GameElement.{Player, Position}
import GameInterface.Render
import GameUtilities.RoundUtil.playerVsAi

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.Random

object AiLevelMatch {

  def AiVsAi(turn : Int, roundType : String,r:Random,shipClass : List[(String,Int)],roundNumber :Int,score1 :Int,score2:Int): Unit = {

    if(roundType == "easy vs medium")
      {
        val easyAi = AiInGameHandler.generatePlayerWithItsShip(shipClass,r, "easy ai")
        val mediumAi = AiInGameHandler.generatePlayerWithItsShip(shipClass, r, "medium ai")
        if (turn == 0) {
          val gameState = GameState(easyAi, mediumAi)
          easyVsMedium(r,gameState,(List.empty[Position],""))
        }
        else {
          val gameState = GameState(mediumAi,easyAi)
          easyVsMedium(r,gameState,(List.empty[Position],""))
        }
      }
    else if(roundType == "easy vs hard")
      {
        val easyAi = AiInGameHandler.generatePlayerWithItsShip(shipClass,r, "easy ai")
        val hardAi = AiInGameHandler.generatePlayerWithItsShip(shipClass, r, "hard ai")
        if (turn == 0) {
          val gameState = GameState(easyAi, hardAi)
          easyVsHard(r,gameState,((List.empty[Position],""),(List.empty[Position],"")),false)
        }
        else {
          val gameState = GameState(hardAi, easyAi)
          easyVsHard(r,gameState,((List.empty[Position],""),(List.empty[Position],"")),false)
        }
      }
    else
    {

      val mediumAi = AiInGameHandler.generatePlayerWithItsShip(shipClass, r, "medium ai")
      val hardAi = AiInGameHandler.generatePlayerWithItsShip(shipClass, r, "hard ai")
      if (turn == 0) {
        val gameState = GameState(mediumAi, hardAi)

        mediumVsHard(r,gameState,((List.empty[Position],""),(List.empty[Position],"")),false,(List.empty[Position],""))
      }
      else {
        val gameState = GameState(hardAi,mediumAi)
        mediumVsHard(r,gameState,((List.empty[Position],""),(List.empty[Position],"")),false,(List.empty[Position],""))
      }
    }

    @tailrec
    def easyVsMedium(r:Random,gameState : GameState,nextTarget :(List[Position],String)): Unit =
    {
      val positionShot = {
        if (gameState.currentPlayer.playerType == "easy ai")
          AiInGameHandler.aiMakeAShot(gameState.currentPlayer,r)
        else
          {
            if(nextTarget._1.isEmpty)
              AiInGameHandler.aiMakeAShot(gameState.currentPlayer,r)
            else
            {
              nextTarget._1.head
            }
          }
      }

      if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == -1)
      {
        val newCurrentPlayer = Player.missShot(gameState.currentPlayer,positionShot)

        val newNextPlayer = Player.receiveMissShot(gameState.nextPlayer,positionShot)

        val newGameState = GameState(newNextPlayer,newCurrentPlayer)

        if(gameState.currentPlayer.playerType == "medium ai")
        {
          val newNextTarget = {
            if(nextTarget._1.isEmpty)
              nextTarget
            else
              (nextTarget._1.tail,nextTarget._2)
          }
          easyVsMedium(r,newGameState,newNextTarget)
        }
        else
          easyVsMedium(r,newGameState,nextTarget)

      }
      else if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == 0)
      {
        val newCurrentPlayer = Player.successShot(gameState.currentPlayer,positionShot)
        val newNextPlayer = Player.receiveSuccessShot(gameState.nextPlayer,positionShot)
        if(!Player.checkShipState(newNextPlayer,positionShot))
        {
          if(Player.checkFleetState(newNextPlayer.fleet))
          {
            val newGameState = GameState(newNextPlayer,newCurrentPlayer)
            if(gameState.currentPlayer.playerType == "medium ai")
            {
              val newNextTarget = (List.empty[Position],"")
              easyVsMedium(r,newGameState,newNextTarget)
            }
            else
              easyVsMedium(r,newGameState,nextTarget)
          }
          else
          {
            val newTurn = {
              if(turn == 0)
                1
              else
                0
            }
            if(roundNumber < 100)
              {

                if(gameState.currentPlayer.playerType =="easy ai")
                  AiVsAi(newTurn,roundType,r,shipClass,roundNumber+1,score1+1,score2)
                else
                  AiVsAi(newTurn,roundType,r,shipClass,roundNumber+1,score1,score2+1)
              }

            else
            {
              println("Entering Easy vs Hard")
              println("easy ai : "+score1+" medium ai : "+score2)
              StdIn.readLine()
              AiVsAi(0,"easy vs hard",r,shipClass,0,0,0)
            }
          }
        }
        else
        {
          val newGameState = GameState(newNextPlayer,newCurrentPlayer)
          if(gameState.currentPlayer.playerType == "easy ai")
          {
            easyVsMedium(r,newGameState,nextTarget)
          }
          else {
            val orientation = {
              if (nextTarget._1.nonEmpty)
                nextTarget._2
              else {
                val randomOrientation = r.nextInt(2)
                if (randomOrientation == 0)
                  "H"
                else
                  "V"
              }
            }
            val positionsToTarget = GameHandlerUtil.middleAiGetNextPosition(positionShot, orientation, newCurrentPlayer.enemyGrid)
            val nextPositions = {
              if (nextTarget._1.isEmpty)
                positionsToTarget
              else
                nextTarget._1.tail ++ positionsToTarget
            }
            val newNextTarget = (nextPositions, orientation)
            easyVsMedium(r, newGameState, newNextTarget)
          }
        }
      }
    }

    @tailrec
    def easyVsHard(r:Random,gameState : GameState,nextTarget : ((List[Position],String),(List[Position],String)),
                       checkHorizontal : Boolean)
    {
      val positionShot = {
        if (gameState.currentPlayer.playerType == "easy ai")
        {
          AiInGameHandler.aiMakeAShot(gameState.currentPlayer,r)
        }
        else
        {
          if(nextTarget._1._1.isEmpty && nextTarget._2._1.isEmpty)
            AiInGameHandler.aiMakeAShot(gameState.currentPlayer,r)
          else
          {
            GameHandlerUtil.hardAiGetNextShotPosition(nextTarget)
          }
        }

      }
      if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == -1)
      {
        val newCurrentPlayer = Player.missShot(gameState.currentPlayer,positionShot)

        val newNextPlayer = Player.receiveMissShot(gameState.nextPlayer,positionShot)

        val newGameState = GameState(newNextPlayer,newCurrentPlayer)

        if(gameState.currentPlayer.playerType == "easy ai")
          easyVsHard(r,newGameState,nextTarget,checkHorizontal)
        else
        {
          val newNextTarget = GameHandlerUtil.deleteUsedTargetByHardAi(nextTarget,positionShot)
          val checkHorizontalUpdated =
          {
            if(newNextTarget._2._1.isEmpty)
              false
            else
              checkHorizontal
          }
          easyVsHard(r,newGameState,newNextTarget,checkHorizontalUpdated)
        }
      }
      else if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == 0)
      {
        val newCurrentPlayer = Player.successShot(gameState.currentPlayer,positionShot)
        val newNextPlayer = Player.receiveSuccessShot(gameState.nextPlayer,positionShot)
        if(!Player.checkShipState(newNextPlayer,positionShot))
        {
          if(Player.checkFleetState(newNextPlayer.fleet))
          {
            val newGameState = GameState(newNextPlayer,newCurrentPlayer)
            if(gameState.currentPlayer.playerType == "hard ai")
            {
              val newNextTarget = GameHandlerUtil.deleteUsedTargetByHardAi(nextTarget,positionShot)
              val checkHorizontalUpdate = newNextTarget._2._1.nonEmpty
              easyVsHard(r,newGameState,newNextTarget,checkHorizontalUpdate)
            }
            else
              easyVsHard(r,newGameState,nextTarget,checkHorizontal)
          }
          else
          {
            val newTurn = {
              if(turn == 0)
                1
              else
                0
            }
            if(roundNumber < 100)
            {

              if(gameState.currentPlayer.playerType =="easy ai")
                AiVsAi(newTurn,roundType,r,shipClass,roundNumber+1,score1+1,score2)
              else
                AiVsAi(newTurn,roundType,r,shipClass,roundNumber+1,score1,score2+1)
            }

            else
            {

              println("Entering medium vs Hard")
              println("easy ai : "+score1+" hard ai : "+score2)
              StdIn.readLine()
              AiVsAi(0,"medium vs hard",r,shipClass,0,0,0)
            }
          }
        }
        else
        {
          val newGameState = GameState(newNextPlayer,newCurrentPlayer)
          if(gameState.currentPlayer.playerType == "easy ai")
          {
            easyVsHard(r,newGameState,nextTarget,checkHorizontal)
          }
          else if(gameState.currentPlayer.playerType == "hard ai")
          {
            if(nextTarget._1._1.isEmpty && nextTarget._2._1.nonEmpty && checkHorizontal)
            {
              val horizontalPositions = GameHandlerUtil.middleAiGetNextPosition(positionShot,"H",newCurrentPlayer.enemyGrid)
              val checkHorizontalUpdate = false
              val verticalTarget = GameHandlerUtil.getNextPositionToTargetForHardAi(nextTarget,positionShot,newCurrentPlayer.enemyGrid)
              val newNextTarget = ((horizontalPositions,"H"),verticalTarget._2)
              easyVsHard(r,newGameState,newNextTarget,checkHorizontalUpdate)

            }
            else
            {
              val newNextTarget = GameHandlerUtil.getNextPositionToTargetForHardAi(nextTarget,positionShot,newCurrentPlayer.enemyGrid)
              easyVsHard(r,newGameState,newNextTarget,checkHorizontal)
            }

          }
        }
      }
    }

    @tailrec
    def mediumVsHard(r:Random,gameState : GameState,nextTarget : ((List[Position],String),(List[Position],String)),
                   checkHorizontal : Boolean,mediumNextTarget :(List[Position],String))
    {
      val positionShot = {
        if (gameState.currentPlayer.playerType == "medium ai")
        {
          if(mediumNextTarget._1.isEmpty)
            AiInGameHandler.aiMakeAShot(gameState.currentPlayer,r)
          else
          {
            mediumNextTarget._1.head
          }
        }
        else
        {
          if(nextTarget._1._1.isEmpty && nextTarget._2._1.isEmpty)
            AiInGameHandler.aiMakeAShot(gameState.currentPlayer,r)
          else
          {
            GameHandlerUtil.hardAiGetNextShotPosition(nextTarget)
          }
        }
      }
      if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == -1)
      {
        val newCurrentPlayer = Player.missShot(gameState.currentPlayer,positionShot)

        val newNextPlayer = Player.receiveMissShot(gameState.nextPlayer,positionShot)

        val newGameState = GameState(newNextPlayer,newCurrentPlayer)

        if(gameState.currentPlayer.playerType == "medium ai")
          {
            val newMediumNextTarget = {
              if(mediumNextTarget._1.isEmpty)
                mediumNextTarget
              else
                (mediumNextTarget._1.tail,mediumNextTarget._2)
            }
            mediumVsHard(r,newGameState,nextTarget,checkHorizontal,newMediumNextTarget)
          }
        else
        {
          val newNextTarget = GameHandlerUtil.deleteUsedTargetByHardAi(nextTarget,positionShot)
          val checkHorizontalUpdated =
          {
            if(newNextTarget._2._1.isEmpty)
              false
            else
              checkHorizontal
          }
          mediumVsHard(r,newGameState,newNextTarget,checkHorizontalUpdated,mediumNextTarget)
        }
      }
      else if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == 0)
      {
        val newCurrentPlayer = Player.successShot(gameState.currentPlayer,positionShot)
        val newNextPlayer = Player.receiveSuccessShot(gameState.nextPlayer,positionShot)
        if(!Player.checkShipState(newNextPlayer,positionShot))
        {
          if(Player.checkFleetState(newNextPlayer.fleet))
          {
            val newGameState = GameState(newNextPlayer,newCurrentPlayer)
            if(gameState.currentPlayer.playerType == "hard ai")
            {
              val newNextTarget = GameHandlerUtil.deleteUsedTargetByHardAi(nextTarget,positionShot)
              val checkHorizontalUpdate = newNextTarget._2._1.nonEmpty
              mediumVsHard(r,newGameState,newNextTarget,checkHorizontalUpdate,mediumNextTarget)
            }
            else
            {
              val newMediumNextTarget = (List.empty[Position],"")
              mediumVsHard(r,newGameState,nextTarget,checkHorizontal,newMediumNextTarget)
            }
          }
          else
          {
            val newTurn = {
              if(turn == 0)
                1
              else
                0
            }
            if(roundNumber < 100)
            {

              if(gameState.currentPlayer.playerType == "medium ai")
                AiVsAi(newTurn,roundType,r,shipClass,roundNumber+1,score1+1,score2)
              else
                AiVsAi(newTurn,roundType,r,shipClass,roundNumber+1,score1,score2+1)
            }

            else
            {

              println("medium ai : "+score1+" hard ai : "+score2)
              StdIn.readLine()
            }
          }
        }
        else
        {
          val newGameState = GameState(newNextPlayer,newCurrentPlayer)
          if(gameState.currentPlayer.playerType == "medium ai")
          {
            val orientation = {
              if (mediumNextTarget._1.nonEmpty)
                mediumNextTarget._2
              else {
                val randomOrientation = r.nextInt(2)
                if (randomOrientation == 0)
                  "H"
                else
                  "V"
              }
            }
            val positionsToTarget = GameHandlerUtil.middleAiGetNextPosition(positionShot, orientation, newCurrentPlayer.enemyGrid)
            val nextPositions = {
              if (mediumNextTarget._1.isEmpty)
                positionsToTarget
              else
                mediumNextTarget._1.tail ++ positionsToTarget
            }
            val newMediumNextTarget = (nextPositions, orientation)
            mediumVsHard(r, newGameState, nextTarget,checkHorizontal,newMediumNextTarget)
          }
          else if(gameState.currentPlayer.playerType == "hard ai")
          {
            if(nextTarget._1._1.isEmpty && nextTarget._2._1.nonEmpty && checkHorizontal)
            {
              val horizontalPositions = GameHandlerUtil.middleAiGetNextPosition(positionShot,"H",newCurrentPlayer.enemyGrid)
              val checkHorizontalUpdate = false
              val verticalTarget = GameHandlerUtil.getNextPositionToTargetForHardAi(nextTarget,positionShot,newCurrentPlayer.enemyGrid)
              val newNextTarget = ((horizontalPositions,"H"),verticalTarget._2)
              mediumVsHard(r,newGameState,newNextTarget,checkHorizontalUpdate,mediumNextTarget)

            }
            else
            {
              val newNextTarget = GameHandlerUtil.getNextPositionToTargetForHardAi(nextTarget,positionShot,newCurrentPlayer.enemyGrid)
              mediumVsHard(r,newGameState,newNextTarget,checkHorizontal,mediumNextTarget)
            }

          }
        }
      }
    }


  }

}
