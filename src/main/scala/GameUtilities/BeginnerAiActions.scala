package GameUtilities

import GameElement.Player
import scala.io.StdIn
import scala.util.Random

/**
  * This class will handle the behaviour of the easy Ai
  */
object BeginnerAiActions {

  /**
    * The behaviour of the easy Ai when it is its turn to make a shot
    * @param gameState : the game state that have the different information on the game's player
    * @param r : the random variable that the Ai will use
    * @return : A new game state that is updated according to the action of the Ai
    */
  def beginnerAiTurn(gameState : GameState,r : Random) : GameState =
  {
    val positionShot = GeneralActionsForAi.aiMakeAShot(gameState.currentPlayer,r)
    if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == -1)
    {
      val newCurrentPlayer = Player.missShot(gameState.currentPlayer,positionShot)
      val newNextPlayer = Player.receiveMissShot(gameState.nextPlayer,positionShot)
      GameState(newNextPlayer,newCurrentPlayer)
    }
    else
    {
      val newCurrentPlayer = Player.successShot(gameState.currentPlayer,positionShot)
      val newNextPlayer = Player.receiveSuccessShot(gameState.nextPlayer,positionShot)
      if(!Player.checkShipState(newNextPlayer,positionShot))
      {
        if (gameState.currentPlayer.playerType == "ai")
        {
          println("Ship Destroyed!!!!!")
          StdIn.readLine("Press any key to continue")
        }
      }
      GameState(newNextPlayer,newCurrentPlayer)
    }
  }
}
