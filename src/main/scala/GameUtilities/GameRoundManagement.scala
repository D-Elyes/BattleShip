package GameUtilities

import Game.BattleShip.{Game}
import GameElement.{Player, Position}

import scala.annotation.tailrec
import scala.util.Random

/**
  *
  */
object GameRoundManagement {

  /**
    *
    * @param turn
    * @param shipClass
    */
  def playerVsPlayer(turn : Boolean, shipClass : List[(String,Int)]) {
    println("First Player ship settings")
    val player1 = UserAsPlayerShipsInit.generatePlayerWithItsShip(shipClass, "player 1")
    println("Second Player setting")
    val player2 = UserAsPlayerShipsInit.generatePlayerWithItsShip(shipClass, "player 2")

    if (turn) {
      println("Player 1 Starts")
      val gameState = GameState(player1, player2)
      battle(gameState)
    }
    else {
      println("Player 2 start the attack phase")
      val gameState = GameState(player2, player1)
      battle(gameState)
    }

    /**
      *
      * @param gameState
      */
    @tailrec
    def battle(gameState: GameState) {
      if (Player.checkFleetState(gameState.currentPlayer.fleet)) {
        println(gameState.currentPlayer.playerType + " : ")
        val updateGameState = UserAsPlayerInGameActions.userTurn(gameState)
        battle(updateGameState)
      }
      else {
        println("Congratulation !!! you just destroyed the enemy's last ship!!!!")
        val repeat = GeneralGameManagement.endGameInput()
        if (repeat) {
          playerVsPlayer(!turn, shipClass)
        }
        else {
          Game()
        }
      }
    }
  }

  /**
    *
    * @param turn
    * @param level
    * @param r
    * @param shipClass
    */
  def playerVsAi(turn : Boolean, level : Int,r:Random,shipClass : List[(String,Int)]): Unit =
  {
    val player = UserAsPlayerShipsInit.generatePlayerWithItsShip(shipClass,"player")
    val ai = GeneralActionsForAi.generatePlayerWithItsShip(shipClass,r,"ai")
    if(turn)
      {
        println("Player begin the attack")
        val gameState = GameState(player,ai)
        if(level==1)
          playerVsAiEasy(r,gameState)
        else if(level == 2)
          playerVsAiMedium(r,gameState,(List.empty[Position],""))
        else
          playerVsAiHard(r,gameState,((List.empty[Position],""),(List.empty[Position],"")),false)
      }
    else
      {
        println("Ai begin the Attack")
        val gameState = GameState(ai,player)
        if(level==1)
          playerVsAiEasy(r,gameState)
        else if(level == 2)
          playerVsAiMedium(r,gameState,(List.empty[Position],""))
        else
          playerVsAiHard(r,gameState,((List.empty[Position],""),(List.empty[Position],"")),false)
      }

    /**
      *
      * @param r
      * @param gameState
      */
    @tailrec
    def playerVsAiEasy(r:Random,gameState : GameState): Unit = {

      if (Player.checkFleetState(gameState.currentPlayer.fleet)) {
        if (gameState.currentPlayer.playerType == "player") {
          val updateGameState = UserAsPlayerInGameActions.userTurn(gameState)
          playerVsAiEasy(r, updateGameState)
        }
        else {
          val updateGameState = BeginnerAiActions.beginnerAiTurn(gameState, r)
          playerVsAiEasy(r,updateGameState)
        }
      }
      else {
        println("Congratulation !!! you just destroyed the enemy's last ship!!!!")
        val repeat = GeneralGameManagement.endGameInput()
        if (repeat) {
          playerVsPlayer(!turn, shipClass)
        }
        else {
          Game()
        }
      }
    }

    /**
      *
      * @param r
      * @param gameState
      * @param nextTarget
      */
    @tailrec
    def playerVsAiMedium(r:Random,gameState : GameState,nextTarget :(List[Position],String))
    {
      if (Player.checkFleetState(gameState.currentPlayer.fleet))
        {
          if (gameState.currentPlayer.playerType == "player") {
            val updateGameState = UserAsPlayerInGameActions.userTurn(gameState)
            playerVsAiMedium(r, updateGameState,nextTarget)
          }
          else
            {
              val aiMediumTurn = MediumAiActions.mediumAiTurn(gameState,r,nextTarget)
              playerVsAiMedium(r,aiMediumTurn._1,aiMediumTurn._2)
            }
        }
      else
        {
          println("Congratulation !!! you just destroyed the enemy's last ship!!!!")
          val repeat = GeneralGameManagement.endGameInput()
          if (repeat) {
            playerVsPlayer(!turn, shipClass)
          }
          else {
            Game()
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
    def playerVsAiHard(r:Random,gameState : GameState,nextTarget : ((List[Position],String),(List[Position],String)),
                       checkHorizontal : Boolean)
    {
      if (Player.checkFleetState(gameState.currentPlayer.fleet))
      {
        if (gameState.currentPlayer.playerType == "player") {
          val updateGameState = UserAsPlayerInGameActions.userTurn(gameState)
          playerVsAiHard(r, updateGameState,nextTarget,checkHorizontal)
        }
        else
        {
          val aiHardTurn = HardAiActions.aiHardTurn(r,gameState,nextTarget,checkHorizontal)
          playerVsAiHard(r,aiHardTurn._1,aiHardTurn._2._1,aiHardTurn._2._2)
        }
      }
      else
      {
        println("Congratulation !!! you just destroyed the enemy's last ship!!!!")
        val repeat = GeneralGameManagement.endGameInput()
        if (repeat) {
          playerVsPlayer(!turn, shipClass)
        }
        else {
          Game()
        }
      }
    }
  }
}
