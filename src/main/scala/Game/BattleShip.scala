package Game

import GameInterface.Render

import scala.annotation.tailrec
import scala.io.StdIn

object BattleShip extends App {


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

        }
        case "2" =>
        case "3" =>
        case "4" =>
        case "5" =>{
          println("Au revoir!!!!!!!!!")
        }
        case _ =>{
          println("Choice error !!!! choose one of the option by entering its number (1,2,3,4 or 5)")
          mainLoop()
        }
      }

    }
  }

}
