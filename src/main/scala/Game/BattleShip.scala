package Game

import GameElement.Player
import GameInterface.Render
import GameUtilities.{AiLevelMatch, GeneralGameManagement, GameRoundManagement}

import scala.annotation.tailrec
import scala.util.Random

object BattleShip extends App {

  val shipClass = List.apply(("Carrier",5),("Battleship",4),("Cruiser",3),("Submarine",3),("Destroyer",2))
  val r = Random
  Game()



  def Game()
  {
    println("Welcome to Battle Ship\n\n")
    mainLoop()
    @tailrec
    def mainLoop(): Unit =
    {
      Render.menuRederer()
      val gameModeChoice = GeneralGameManagement.gameModeChoice()
      gameModeChoice match
      {
        case "1" =>{
          GameRoundManagement.playerVsPlayer(true,shipClass)
        }
        case "2" =>{
          Render.aiLevelChoice()
          val choiceLevel = GeneralGameManagement.aiLevelInput()
          GameRoundManagement.playerVsAi(true,choiceLevel,r,shipClass)
        }
        case "3" =>{
          AiLevelMatch.AiVsAi(true,"beginner vs medium",r,shipClass,0,0,0,List.empty[((String,Int),(String,Int))])
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
