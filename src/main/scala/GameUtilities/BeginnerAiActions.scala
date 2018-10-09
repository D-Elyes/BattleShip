package GameUtilities

import GameElement.Player

import scala.io.StdIn
import scala.util.Random

object BeginnerAiActions {

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
