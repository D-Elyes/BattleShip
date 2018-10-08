package Game

import GameElement.Player
import GameInterface.Render
import GameUtilities.{AiLevelMatch, GameHandlerUtil, RoundUtil}

import scala.annotation.tailrec
import scala.util.Random

object BattleShip extends App {

  case class GameState(currentPlayer : Player, nextPlayer : Player)
  val shipClass = List.apply(("Carrier",5),("Battleship",4),("Cruiser",3),("Submarine",3),("Destroyer",2))
  val r = Random

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
      val gameModeChoice = GameHandlerUtil.gameModeChoice()
      gameModeChoice match
      {
        case "1" =>{
          RoundUtil.playerVsPlayer(0,shipClass)
        }
        case "2" =>{
          Render.aiLevelChoice()
          val choiceLevel = GameHandlerUtil.aiLevelInput()
          RoundUtil.playerVsAi(0,choiceLevel,r,shipClass)
        }
        case "3" =>{
          AiLevelMatch.AiVsAi(0,"easy vs medium",r,shipClass,0,0,0,List.empty[((String,Int),(String,Int))])
        }
        case "4" =>{
          Render.gameClose()
        }
        case _ =>{
          println("Choice error !!!! choose one of the option by entering its number (1,2,3 or 4)")
          mainLoop()
        }
      }

    }
  }



}
