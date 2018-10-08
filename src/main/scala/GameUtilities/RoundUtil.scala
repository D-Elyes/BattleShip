package GameUtilities

import Game.BattleShip.{Game, GameState}
import GameElement.{Player, Position}
import GameInterface.Render

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.Random

object RoundUtil {


  def playerVsPlayer(turn : Int, shipClass : List[(String,Int)])
  {
    println("First Player ship settings")
    val player1 = PlayerInGameHandler.generatePlayerWithItsShip(shipClass,"player 1")
    println("Second Player setting")
    val player2 = PlayerInGameHandler.generatePlayerWithItsShip(shipClass,"player 2")

    if(turn ==0) {
      println("Player 1 Starts the attack pahse")
      val gameState = GameState(player1, player2)
      battle(gameState)
    }
    else
    {
      println("Player 2 start the attack phase")
      val gameState = GameState(player2, player1)
      battle(gameState)
    }
    @tailrec
    def battle(gameState : GameState)
    {
      println("Attack phase !!"+gameState.currentPlayer.playerType+" procedding to attack")
      println(gameState.currentPlayer.playerType+" grid : ")
      Render.playerGridRenderer(gameState.currentPlayer.ownGrid)
      println(gameState.currentPlayer.playerType+" enemy grid : ")
      Render.playerGridRenderer(gameState.currentPlayer.enemyGrid)
      val positionShot = PlayerInGameHandler.playerMakeAShot(gameState.currentPlayer)
      if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == -1)
      {
        println("Miss Shot !!!!!")
        val newCurrentPlayer = Player.missShot(gameState.currentPlayer,positionShot)
        Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
        val newNextPlayer = Player.receiveMissShot(gameState.nextPlayer,positionShot)
        val newGameState = GameState(newNextPlayer,newCurrentPlayer)
        println("Moving to defense phase ")
        println("****************")
        println("Entering "+gameState.nextPlayer.playerType +" turn")
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
            println("Entering "+ gameState.nextPlayer.playerType +" turn")
            StdIn.readLine("Press key to continue")
            println()
            battle(newGameState)
          }
          else
          {
            println("Congratulation !!! you just destroyed the enemy's last ship!!!!")
            val repeat = GameHandlerUtil.endGameInput()
            if(repeat)
            {
              if(turn == 0)
                playerVsPlayer(1,shipClass)
              else
                playerVsPlayer(0,shipClass)
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
          println("Entering "+gameState.nextPlayer.playerType +" turn")
          println("Press any key to continue")
          StdIn.readLine()
          println()
          battle(newGameState)
        }
      }
    }
  }

  def playerVsAi(turn : Int, level : Int,r:Random,shipClass : List[(String,Int)]): Unit =
  {
    println("First Player ship settings")
    val player = PlayerInGameHandler.generatePlayerWithItsShip(shipClass,"player")
    println("AI ship settings")
    val ai = AiInGameHandler.generatePlayerWithItsShip(shipClass,r,"ai")
    if(turn == 0)
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

    @tailrec
    def playerVsAiEasy(r:Random,gameState : GameState): Unit =
    {
      println("Attack phase !!"+gameState.currentPlayer.playerType +" procedding to attack")
      if(gameState.currentPlayer.playerType == "player")
        {
          println("Player grid : ")
          Render.playerGridRenderer(gameState.currentPlayer.ownGrid)
          println("Player's enemy grid : ")
          Render.playerGridRenderer(gameState.currentPlayer.enemyGrid)

        }
      val positionShot = {
        if (gameState.currentPlayer.playerType == "player")
          PlayerInGameHandler.playerMakeAShot(gameState.currentPlayer)
        else
          AiInGameHandler.aiMakeAShot(gameState.currentPlayer,r)
      }
      if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == -1)
      {
        println("Miss Shot !!!!!")
        val newCurrentPlayer = Player.missShot(gameState.currentPlayer,positionShot)

        val newNextPlayer = Player.receiveMissShot(gameState.nextPlayer,positionShot)

        val newGameState = GameState(newNextPlayer,newCurrentPlayer)

        if(gameState.currentPlayer.playerType == "player")
          {

            Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
            println("Moving to defense phase ")
            println("****************")
            println("Entering"+ gameState.nextPlayer.playerType +" turn")
            StdIn.readLine("Press key to continue")
            println()
          }

        playerVsAiEasy(r,newGameState)
      }
      else if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == 0)
      {
        println("Shot Succeeded !!!!!")
        val newCurrentPlayer = Player.successShot(gameState.currentPlayer,positionShot)
        if(gameState.currentPlayer.playerType=="player")
          {
            Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
          }
        val newNextPlayer = Player.receiveSuccessShot(gameState.nextPlayer,positionShot)
        if(!Player.checkShipState(newNextPlayer,positionShot))
        {
          println("Ship Destroyed!!!!!")
          if(Player.checkFleetState(newNextPlayer.fleet))
          {
            val newGameState = GameState(newNextPlayer,newCurrentPlayer)
            if(gameState.currentPlayer.playerType == "player")
              {
                println("Moving to defense phase ")
                println("****************")
                println("Entering "+ gameState.nextPlayer.playerType +" turn")
                StdIn.readLine("Press key to continue")
                println()
              }

            playerVsAiEasy(r,newGameState)
          }
          else
          {
            println("Congratulation !!! you just destroyed the enemy's last ship!!!!")
            val repeat = GameHandlerUtil.endGameInput()
            if(repeat)
            {
              if(turn == 0)
                playerVsAi(1,level,r,shipClass)
              else
                playerVsAi(0,level,r,shipClass)
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
          if(gameState.currentPlayer.playerType == "player")
            {
              println("Moving to defense phase ")
              println("****************")
              println("Entering "+ gameState.nextPlayer.playerType +" turn")
              println("Press any key to continue")
              StdIn.readLine()
              println()
            }

          playerVsAiEasy(r,newGameState)
        }
      }
    }

    @tailrec
    def playerVsAiMedium(r:Random,gameState : GameState,nextTarget :(List[Position],String))
    {
      println("Attack phase !!"+ gameState.currentPlayer.playerType +" procedding to attack")
      if(gameState.currentPlayer.playerType == "player")
      {
        println("Player grid : ")
        Render.playerGridRenderer(gameState.currentPlayer.ownGrid)
        println("Players's enemy grid : ")
        Render.playerGridRenderer(gameState.currentPlayer.enemyGrid)

      }
      val positionShot = {
        if (gameState.currentPlayer.playerType == "player")
          {
            PlayerInGameHandler.playerMakeAShot(gameState.currentPlayer)
          }
        else
        {
          if(nextTarget._1.isEmpty)
            AiInGameHandler.aiMakeAShot(gameState.currentPlayer,r)
          else
            {
              nextTarget._1.head
            }
        }

      }
      if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == -1)
      {
        println("Miss Shot !!!!!")

        val newCurrentPlayer = Player.missShot(gameState.currentPlayer,positionShot)

        val newNextPlayer = Player.receiveMissShot(gameState.nextPlayer,positionShot)

        val newGameState = GameState(newNextPlayer,newCurrentPlayer)

        if(gameState.currentPlayer.playerType == "player")
        {

          Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
          println("Moving to defense phase ")
          println("****************")
          println("Entering "+ gameState.nextPlayer.playerType +" turn")
          StdIn.readLine("Press key to continue")
          println()
        }
        if(gameState.currentPlayer.playerType == "player")
           playerVsAiMedium(r,newGameState,nextTarget)
        else
          {
            val newNextTarget = {
              if(nextTarget._1.isEmpty)
                nextTarget
              else
                (nextTarget._1.tail,nextTarget._2)
            }
            playerVsAiMedium(r,newGameState,newNextTarget)
          }
      }
      else if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == 0)
      {
        println("Shot Succeeded !!!!!")
        val newCurrentPlayer = Player.successShot(gameState.currentPlayer,positionShot)
        if(gameState.currentPlayer.playerType =="player")
        {
          Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
        }
        val newNextPlayer = Player.receiveSuccessShot(gameState.nextPlayer,positionShot)
        if(!Player.checkShipState(newNextPlayer,positionShot))
        {
          println("Ship Destroyed!!!!!")

          if(Player.checkFleetState(newNextPlayer.fleet))
          {
            val newGameState = GameState(newNextPlayer,newCurrentPlayer)
            if(gameState.currentPlayer.playerType == "player")
            {
              println("Moving to defense phase ")
              println("****************")
              println("Entering "+ gameState.nextPlayer.playerType +" turn")
              StdIn.readLine("Press key to continue")
              println()
            }

            if(gameState.currentPlayer.playerType == "ai")
              {
                val newNextTarget = (List.empty[Position],"")
                playerVsAiMedium(r,newGameState,newNextTarget)
              }
            else
              playerVsAiMedium(r,newGameState,nextTarget)
          }
          else
          {
            println("Congratulation !!! you just destroyed the enemy's last ship!!!!")
            val repeat = GameHandlerUtil.endGameInput()
            if(repeat)
            {
              if(turn == 0)
                playerVsAi(1,level,r,shipClass)
              else
                playerVsAi(0,level,r,shipClass)
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
          if(gameState.currentPlayer.playerType == "player")
          {
            println("Moving to defense phase ")
            println("****************")
            println("Entering "+ gameState.nextPlayer.playerType +" turn")
            println("Press any key to continue")
            StdIn.readLine()
            println()
            playerVsAiMedium(r,newGameState,nextTarget)
          }
          else if(gameState.currentPlayer.playerType == "ai")
            {
              val orientation = {
                if(nextTarget._1.nonEmpty)
                  nextTarget._2
                else
                {
                  val randomOrientation = r.nextInt(2)
                  if(randomOrientation == 0)
                    "H"
                  else
                    "V"
                }
              }
              val positionsToTarget = GameHandlerUtil.middleAiGetNextPosition(positionShot,orientation,newCurrentPlayer.enemyGrid)
              val nextPositions = {
                if(nextTarget._1.isEmpty)
                  positionsToTarget
                else
                  nextTarget._1.tail ++ positionsToTarget
              }
              val newNextTarget = (nextPositions,orientation)
              playerVsAiMedium(r,newGameState,newNextTarget)
            }
        }
      }
    }

    @tailrec
    def playerVsAiHard(r:Random,gameState : GameState,nextTarget : ((List[Position],String),(List[Position],String)),
                       checkHorizontal : Boolean)
    {
      println("Attack phase !!"+ gameState.currentPlayer.playerType +" procedding to attack")
      if(gameState.currentPlayer.playerType == "player")
      {
        println("Player grid : ")
        Render.playerGridRenderer(gameState.currentPlayer.ownGrid)
        println("Player's enemy grid : ")
        Render.playerGridRenderer(gameState.currentPlayer.enemyGrid)

      }
      val positionShot = {
        if (gameState.currentPlayer.playerType == "player")
        {
          PlayerInGameHandler.playerMakeAShot(gameState.currentPlayer)
        }
        else
        {
          if(nextTarget._1._1.isEmpty && nextTarget._2._1.isEmpty)
            AiInGameHandler.aiMakeAShot(gameState.currentPlayer,r)
          else
          {
            GameHandlerUtil.hardAiGetNextShotPosition(nextTarget)
          }
        }

      }
      println("*************"+positionShot)
      if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == -1)
      {
        println("Miss Shot !!!!!")

        val newCurrentPlayer = Player.missShot(gameState.currentPlayer,positionShot)

        val newNextPlayer = Player.receiveMissShot(gameState.nextPlayer,positionShot)

        val newGameState = GameState(newNextPlayer,newCurrentPlayer)

        if(gameState.currentPlayer.playerType == "player")
        {

          Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
          println("Moving to defense phase ")
          println("****************")
          println("Entering "+ gameState.nextPlayer.playerType +" turn")
          StdIn.readLine("Press key to continue")
          println()
        }
        if(gameState.currentPlayer.playerType == "player")
          playerVsAiHard(r,newGameState,nextTarget,checkHorizontal)
        else
        {
          val newNextTarget = GameHandlerUtil.deleteUsedTargetByHardAi(nextTarget,positionShot)
          val checkHorizontalUpdated =
          {
            if(newNextTarget._2._1.isEmpty)
              false
            else
              checkHorizontal
          }
          playerVsAiHard(r,newGameState,newNextTarget,checkHorizontalUpdated)
        }
      }
      else if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == 0)
      {
        println("Shot Succeeded !!!!!")
        val newCurrentPlayer = Player.successShot(gameState.currentPlayer,positionShot)
        if(gameState.currentPlayer.playerType =="player")
        {
          Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
        }
        val newNextPlayer = Player.receiveSuccessShot(gameState.nextPlayer,positionShot)
        if(!Player.checkShipState(newNextPlayer,positionShot))
        {
          println("Ship Destroyed!!!!!")

          if(Player.checkFleetState(newNextPlayer.fleet))
          {
            val newGameState = GameState(newNextPlayer,newCurrentPlayer)
            if( gameState.currentPlayer.playerType == "player")
            {
              println("Moving to defense phase ")
              println("****************")
              println("Entering "+ gameState.nextPlayer.playerType +" turn")
              StdIn.readLine("Press key to continue")
              println()
            }
            if(gameState.currentPlayer.playerType == "ai")
            {
              val newNextTarget = GameHandlerUtil.deleteUsedTargetByHardAi(nextTarget,positionShot)
              val checkHorizontalUpdate = newNextTarget._2._1.nonEmpty
              playerVsAiHard(r,newGameState,newNextTarget,checkHorizontalUpdate)
            }
            else
              playerVsAiHard(r,newGameState,nextTarget,checkHorizontal)
          }
          else
          {
            println("Congratulation !!! you just destroyed the enemy's last ship!!!!")
            val repeat = GameHandlerUtil.endGameInput()
            if(repeat)
            {
              if(turn == 0)
                playerVsAi(1,level,r,shipClass)
              else
                playerVsAi(0,level,r,shipClass)
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
          if(gameState.currentPlayer.playerType == "player")
          {
            println("Moving to defense phase ")
            println("****************")
            println("Entering "+ gameState.nextPlayer.playerType +" turn")
            println("Press any key to continue")
            StdIn.readLine()
            println()
            playerVsAiHard(r,newGameState,nextTarget,checkHorizontal)
          }
          else if(gameState.currentPlayer.playerType == "ai")
          {
            if(nextTarget._1._1.isEmpty && nextTarget._2._1.nonEmpty && checkHorizontal)
              {
                val horizontalPositions = GameHandlerUtil.middleAiGetNextPosition(positionShot,"H",newCurrentPlayer.enemyGrid)
                val checkHorizontalUpdate = false
                val verticalTarget = GameHandlerUtil.getNextPositionToTargetForHardAi(nextTarget,positionShot,newCurrentPlayer.enemyGrid)
                val newNextTarget = ((horizontalPositions,"H"),verticalTarget._2)
                playerVsAiHard(r,newGameState,newNextTarget,checkHorizontalUpdate)

              }
            else
              {

                val newNextTarget = GameHandlerUtil.getNextPositionToTargetForHardAi(nextTarget,positionShot,newCurrentPlayer.enemyGrid)
                playerVsAiHard(r,newGameState,newNextTarget,checkHorizontal)
              }

          }
        }
      }
    }

  }




}
