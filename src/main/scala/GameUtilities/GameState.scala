package GameUtilities

import GameElement.Player

/**
  * The GameState class will contains the 2 players of a game, it will contains the  informations about the players
  * @param currentPlayer : the player that will make a shot
  * @param nextPlayer : the player that waits
  */
case class GameState(currentPlayer : Player, nextPlayer : Player)
