package GameUtilities

import GameElement.{Player, Position}
import GameInterface.Render

import scala.io.StdIn

/**
  * This class will contain the functions that the player as a user can do
  * The functions will contain the action of the player when its his turn
  */
object UserAsPlayerInGameActions {

  /**
    * This function will return the position of the player's shot of his turn
    * Here the player is the user
    * @param player : the player that will make the shot
    * @return : the position of the shot
    */
  def playerMakeAShot(player : Player):Position =
  {
    val line = PlayerAsUserInputs.seizeLineNumberShip()
    val column = PlayerAsUserInputs.seizeColumnNumberShip()
    val position = Position(line,column)
    if(player.enemyGrid.grid(position.x)(position.y) == 1 || player.enemyGrid.grid(position.x)(position.y) == 2)
    {
      println("you already shot on this square choose another position!!!")
      playerMakeAShot(player)
    }
    else
    {
      position
    }
  }

  def userTurn(gameState : GameState) : GameState =
  {
    Render.playerGridRenderer(gameState.currentPlayer.ownGrid)
    Render.playerGridRenderer(gameState.currentPlayer.enemyGrid)
    val positionShot = UserAsPlayerInGameActions.playerMakeAShot(gameState.currentPlayer)
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
        println("Ship Destroyed!!!!!")
        StdIn.readLine("Press any key to continue")
      }
     GameState(newNextPlayer,newCurrentPlayer)
    }
  }
}
