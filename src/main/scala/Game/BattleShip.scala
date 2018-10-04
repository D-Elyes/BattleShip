package Game

import GameElement.{Player}
import GameInterface.Render
import GameUtilities.{RoundUtil,GameUtil}

import scala.annotation.tailrec

object BattleShip extends App {

  case class GameState(currentPlayer : Player, nextPlayer : Player)
  val shipClass = List.apply(("Destroyer",2))
  //("Carrier",5),("Battleship",4),("Cruiser",3),("Submarine",3)
  Game()



  def Game()
  {
    println("Welcome to Battle Ship\n\n")
    mainLoop()
    @tailrec
    def mainLoop(): Unit =
    {
      Render.menuRederer()
      val gameModeChoice = GameUtil.gameModeChoice()
      gameModeChoice match
      {
        case "1" =>{
          RoundUtil.playerVsPlayer(0)
        }
        case "2" =>
        case "3" =>
        case "4" =>{
          println("Au Revoir!!!!!!!!!")
        }
        case _ =>{
          println("Choice error !!!! choose one of the option by entering its number (1,2,3 or 4)")
          mainLoop()
        }
      }

    }
  }



}
