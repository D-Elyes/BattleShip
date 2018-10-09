package GameUtilities

import Game.BattleShip.{Game}
import GameElement.{Player, Position}
import scala.annotation.tailrec
import scala.util.Random

/**
  * This class will handle the different round that the user can play
  * He can play vs another player or vs one of the three levels of the Ai
  */
object GameRoundManagement {

  /**
    * This function will handler the round between two users
    * @param turn : represents who's gonna play first : True player 1, false player 2
    *             When the same game is restarted this param changes
    * @param shipClass : the ships that a player can use
    */
  def playerVsPlayer(turn : Boolean, shipClass : List[(String,Int)]) {
    //initialisation of the 2 players
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
      * This function handle the actions of a player when it is his turn to make a shot
      * @param gameState : the game state containing the informations about the two players
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
    * This function contain the three functions that will handle a round of a player vs the three different level of Ai
    *
    * @param turn : represents who's gonna play first : True player , false the Ai
    *             When the same game is restarted this param changes
    * @param level : the level of the Ai
    * @param r : The random variable that the Ais will use
    * @param shipClass : the ships that a player can use
    */
  def playerVsAi(turn : Boolean, level : Int,r:Random,shipClass : List[(String,Int)]): Unit =
  {
    //Initialisation of the player and the Ai
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
      * This function will handle the round between a player and an easy Ai
      * @param r : The random variable that the Ais will use
      * @param gameState : contains the informations of the player and the Ai
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
      * This function will handle the round between a player and a medium Ai
      * @param r : The random variable that the Ais will use
      * @param gameState : contains the informations of the player and the Ai
      * @param nextTarget : The potential targets of the medium Ai
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
      * This function will handle the round between a player and a hard Ai
      * @param r : The random variable that the Ais will use
      * @param gameState : contains the informations of the player and the Ai
      * @param nextTarget : the potential targets of the hard ai
      * @param checkHorizontal : Tells the hard Ai if it should check for the horizontal positions or not
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
