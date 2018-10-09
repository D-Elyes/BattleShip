package GameUtilities

import java.io.{BufferedWriter, FileWriter}

import Game.BattleShip.Game
import GameElement.{Player, Position}

import scala.annotation.tailrec
import scala.util.Random

object AiLevelMatch {

  /**
    *
    * @param turn
    * @param roundType
    * @param r
    * @param shipClass
    * @param roundNumber
    * @param score1
    * @param score2
    * @param scores
    */
  def AiVsAi(turn : Boolean, roundType : String,r:Random,shipClass : List[(String,Int)],
             roundNumber :Int,score1 :Int,score2:Int,scores : List[((String,Int),(String,Int))]): Unit = {

    if(roundType == "beginner vs medium")
      {
        val easyAi = GeneralActionsForAi.generatePlayerWithItsShip(shipClass,r, "beginner ai")
        val mediumAi = GeneralActionsForAi.generatePlayerWithItsShip(shipClass, r, "medium ai")
        if (turn) {
          val gameState = GameState(easyAi, mediumAi)
          easyVsMedium(r,gameState,(List.empty[Position],""))
        }
        else {
          val gameState = GameState(mediumAi,easyAi)
          easyVsMedium(r,gameState,(List.empty[Position],""))
        }
      }
    else if(roundType == "beginner vs hard")
      {
        val easyAi = GeneralActionsForAi.generatePlayerWithItsShip(shipClass,r, "beginner ai")
        val hardAi = GeneralActionsForAi.generatePlayerWithItsShip(shipClass, r, "hard ai")
        if (turn) {
          val gameState = GameState(easyAi, hardAi)
          easyVsHard(r,gameState,((List.empty[Position],""),(List.empty[Position],"")),false)
        }
        else {
          val gameState = GameState(hardAi, easyAi)
          easyVsHard(r,gameState,((List.empty[Position],""),(List.empty[Position],"")),false)
        }
      }
    else if(roundType == "medium vs hard")
    {
      val mediumAi = GeneralActionsForAi.generatePlayerWithItsShip(shipClass, r, "medium ai")
      val hardAi = GeneralActionsForAi.generatePlayerWithItsShip(shipClass, r, "hard ai")
      if (turn) {
        val gameState = GameState(mediumAi, hardAi)

        mediumVsHard(r,gameState,((List.empty[Position],""),(List.empty[Position],"")),false,(List.empty[Position],""))
      }
      else {
        val gameState = GameState(hardAi,mediumAi)
        mediumVsHard(r,gameState,((List.empty[Position],""),(List.empty[Position],"")),false,(List.empty[Position],""))
      }
    }
    else
      {
        val bufferWrite = new BufferedWriter(new FileWriter("ai_proof.csv"))
        bufferWrite.write("AI Name; score; AI Name2; score2\n")
        scores.foreach(x => bufferWrite.write(x._1._1 + "; "+x._1._2+"; "+x._2._1 + "; "+x._2._2+"\n"))
        bufferWrite.close()
        //StdIn.readLine("Press any key to return to menu")
        Game()
      }

    /**
      *
      * @param r
      * @param gameState
      * @param nextTarget
      */
    @tailrec
    def easyVsMedium(r:Random,gameState : GameState,nextTarget :(List[Position],String)): Unit =
    {
      if (Player.checkFleetState(gameState.currentPlayer.fleet)) {
        if (gameState.currentPlayer.playerType == "beginner ai") {
          val updateGameState = BeginnerAiActions.beginnerAiTurn(gameState,r)
          easyVsMedium(r, updateGameState,nextTarget)
        }
        else {
          val updateGameState = MediumAiActions.mediumAiTurn(gameState,r,nextTarget)
          easyVsMedium(r, updateGameState._1,updateGameState._2)
        }
      }
      else {
        if (roundNumber <100) {
          if(gameState.nextPlayer.playerType =="beginner ai")
            AiVsAi(!turn,roundType,r,shipClass,roundNumber+1,score1+1,score2,scores)
          else
            AiVsAi(!turn,roundType,r,shipClass,roundNumber+1,score1,score2+1,scores)
        }
        else {
          if(gameState.nextPlayer.playerType == "beginner ai")
          {
            val newScores = scores :+ (((gameState.nextPlayer.playerType,score1),(gameState.currentPlayer.playerType,score2)))
            AiVsAi(true,"beginner vs hard",r,shipClass,0,0,0,newScores)
          }
          else {
            val newScores = scores :+ ((gameState.nextPlayer.playerType, score2), (gameState.currentPlayer.playerType, score1))
            AiVsAi(true, "beginner vs hard", r, shipClass, 0, 0, 0, newScores)
          }
        }
      }
    }

    /**
      *
      * @param r
      * @param gameState
      * @param nextTarget
      * @param checkHorizontal
      */
    @tailrec
    def easyVsHard(r:Random,gameState : GameState,nextTarget : ((List[Position],String),(List[Position],String)),
                       checkHorizontal : Boolean)
    {
      if (Player.checkFleetState(gameState.currentPlayer.fleet))
      {
        if (gameState.currentPlayer.playerType == "beginner ai") {
          val updateGameState = BeginnerAiActions.beginnerAiTurn(gameState,r)
          easyVsHard(r, updateGameState,nextTarget,checkHorizontal)
        }
        else
        {
          val aiHardTurn = HardAiActions.aiHardTurn(r,gameState,nextTarget,checkHorizontal)
          easyVsHard(r,aiHardTurn._1,aiHardTurn._2._1,aiHardTurn._2._2)
        }
      }
      else {
        if (roundNumber < 100) {

          if (gameState.nextPlayer.playerType == "beginner ai")
            AiVsAi(!turn, roundType, r, shipClass, roundNumber + 1, score1 + 1, score2, scores)
          else
            AiVsAi(!turn, roundType, r, shipClass, roundNumber + 1, score1, score2 + 1, scores)
        }
        else {
          if (gameState.nextPlayer.playerType == "beginner ai") {
            val newScores = scores :+ ((gameState.nextPlayer.playerType, score1), (gameState.currentPlayer.playerType, score2))
            AiVsAi(true, "medium vs hard", r, shipClass, 0, 0, 0, newScores)
          }
          else {
            val newScores = scores :+ ((gameState.nextPlayer.playerType, score2), (gameState.currentPlayer.playerType, score1))
            AiVsAi(true, "medium vs hard", r, shipClass, 0, 0, 0, newScores)
          }
        }
      }
    }

    /**
      *
      * @param r
      * @param gameState
      * @param nextTarget
      * @param checkHorizontal
      * @param mediumNextTarget
      */
    @tailrec
    def mediumVsHard(r:Random,gameState : GameState,nextTarget : ((List[Position],String),(List[Position],String)),
                   checkHorizontal : Boolean,mediumNextTarget :(List[Position],String)): Unit =
    {
      if (Player.checkFleetState(gameState.currentPlayer.fleet))
      {
        if (gameState.currentPlayer.playerType == "medium ai") {
          val updateGameState = MediumAiActions.mediumAiTurn(gameState,r,mediumNextTarget)
          mediumVsHard(r, updateGameState._1,nextTarget,checkHorizontal,updateGameState._2)
        }
        else
        {
          val aiHardTurn = HardAiActions.aiHardTurn(r,gameState,nextTarget,checkHorizontal)
          mediumVsHard(r,aiHardTurn._1,aiHardTurn._2._1,aiHardTurn._2._2,mediumNextTarget)
        }
      }
      else
      {
        if(roundNumber < 100)
        {

          if(gameState.nextPlayer.playerType == "medium ai")
            AiVsAi(!turn,roundType,r,shipClass,roundNumber+1,score1+1,score2,scores)
          else
            AiVsAi(!turn,roundType,r,shipClass,roundNumber+1,score1,score2+1,scores)
        }
        else
        {
          if(gameState.nextPlayer.playerType == "medium ai")
          {
            val newScores = scores :+ ((gameState.nextPlayer.playerType,score1),(gameState.currentPlayer.playerType,score2))
            AiVsAi(true,"Save file",r,shipClass,0,0,0,newScores)
          }
          else
          {
            val newScores = scores :+ ((gameState.nextPlayer.playerType,score2),(gameState.currentPlayer.playerType,score1))
            AiVsAi(true,"Save FIle",r,shipClass,0,0,0,newScores)
          }
        }
      }
    }
  }
}
