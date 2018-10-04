package Game

import GameElement.{Player, Position}
import GameInterface.Render
import GameUtil.PlayerInGameHandler

import scala.annotation.tailrec
import scala.io.StdIn

object BattleShip extends App {

  case class GameState(currentPlayer : Player, nextPlayer : Player)
  val shipClass = List.apply(("Destroyer",2))
  //("Carrier",5),("Battleship",4),("Cruiser",3),("Submarine",3)
  Game()



  def Game()
  {
    println("Welcome to Battle Ship")
    mainLoop()
    @tailrec
    def mainLoop(): Unit =
    {
      Render.menuRederer()
      val gameModeChoice = StdIn.readLine()
      gameModeChoice match
      {
        case "1" =>{
          playerVsPlayer(0)
        }
        case "2" =>
        case "3" =>
        case "4" =>{
          println("Au Revoir!!!!!!!!!")
        }
        case _ =>{
          println("Choice error !!!! choose one of the option by entering its number (1,2,3,4 or 5)")
          mainLoop()
        }
      }

    }
  }

  def playerVsPlayer(turn : Int)
  {
    println("First Player ship settings")
    val player1 = PlayerInGameHandler.generatePlayerWithItsShip(shipClass)
    println("Second Player setting")
    val player2 = PlayerInGameHandler.generatePlayerWithItsShip(shipClass)
    val gameState = GameState(player1,player2)
    if(turn ==0)
      println("Player 1 Starts the attack pahse")
    else
      println("Player 2 start the attack phase")
    battle(gameState)
    @tailrec
    def battle(gameState : GameState)
    {
      println("Attack phase !! procedding to target choice")
      println("Your grid : ")
      Render.playerGridRenderer(gameState.currentPlayer.ownGrid)
      println("Your enemy grid : ")
      Render.playerGridRenderer(gameState.currentPlayer.enemyGrid)
      val positionShot = PlayerInGameHandler.makeAShot(gameState.currentPlayer)
      if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == -1)
        {
          println("Miss Shot !!!!!")
          val newCurrentPlayer = Player.missShot(gameState.currentPlayer,positionShot)
          Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
          val newNextPlayer = Player.receiveMissShot(gameState.nextPlayer,positionShot)
          val newGameState = GameState(newNextPlayer,newCurrentPlayer)
          println("Moving to defense phase ")
          println("****************")
          println("Entering next Player turn")
          StdIn.readLine("Press key to continue")
          println()
          battle(newGameState)
        }
      else if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == 0)
        {
          println("Shot Succeeded !!!!!")
          val newCurrentPlayer = Player.successShot(gameState.currentPlayer,positionShot)
          Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
          val newNextPlayer = Player.receiveSuccessShot(gameState.nextPlayer,positionShot)
          if(!Player.checkShipState(newNextPlayer,positionShot))
          {
            println("Ship Destroyed!!!!!")
            if(Player.checkFleetState(newNextPlayer.fleet))
            {
              val newGameState = GameState(newNextPlayer,newCurrentPlayer)
              println("Moving to defense phase ")
              println("****************")
              println("Entering next Player turn")
              StdIn.readLine("Press key to continue")
              println()
              battle(newGameState)
            }
            else
            {
              println("Congratulation !!! you just destroyed the enemy's last ship!!!!")
              val repeat = PlayerInGameHandler.endGameInput()
              if(repeat)
                {
                  if(turn == 0)
                    playerVsPlayer(1)
                  else
                    playerVsPlayer(0)
                }
              else
                {
                  Game()
                }
            }
          }
          else
            {
              val newGameState = GameState(newNextPlayer,newCurrentPlayer)
              println("Moving to defense phase ")
              println("****************")
              println("Entering next Player turn")
              StdIn.readLine("Press key to continue")
              println()
              battle(newGameState)
            }
        }
    }
  }

}
