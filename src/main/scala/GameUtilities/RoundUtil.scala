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
    val player1 = PlayerInGameHandler.generatePlayerWithItsShip(shipClass)
    println("Second Player setting")
    val player2 = PlayerInGameHandler.generatePlayerWithItsShip(shipClass)

    if(turn ==0) {
      println("Player 1 Starts the attack pahse")
      val gameState = GameState(player1, player2,"player1")
      battle(gameState)
    }
    else
    {
      println("Player 2 start the attack phase")
      val gameState = GameState(player2, player1,"player2")
      battle(gameState)
    }
    @tailrec
    def battle(gameState : GameState)
    {
      println("Attack phase !!"+gameState.turn+" procedding to attack")
      println(gameState.turn+" grid : ")
      Render.playerGridRenderer(gameState.currentPlayer.ownGrid)
      println(gameState.turn+" enemy grid : ")
      Render.playerGridRenderer(gameState.currentPlayer.enemyGrid)
      val positionShot = PlayerInGameHandler.playerMakeAShot(gameState.currentPlayer)
      val nextTurn = GameHandlerUtil.nextTurn(gameState.turn)
      if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == -1)
      {
        println("Miss Shot !!!!!")
        val newCurrentPlayer = Player.missShot(gameState.currentPlayer,positionShot)
        Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
        val newNextPlayer = Player.receiveMissShot(gameState.nextPlayer,positionShot)
        val newGameState = GameState(newNextPlayer,newCurrentPlayer,nextTurn)
        println("Moving to defense phase ")
        println("****************")
        println("Entering "+nextTurn+" turn")
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
            val newGameState = GameState(newNextPlayer,newCurrentPlayer,nextTurn)
            println("Moving to defense phase ")
            println("****************")
            println("Entering "+nextTurn+" turn")
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
          val newGameState = GameState(newNextPlayer,newCurrentPlayer,nextTurn)
          println("Moving to defense phase ")
          println("****************")
          println("Entering "+nextTurn+" turn")
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
    val player = PlayerInGameHandler.generatePlayerWithItsShip(shipClass)
    println("AI ship settings")
    val ai = AiInGameHandler.generatePlayerWithItsShip(shipClass,r)
    Render.playerGridRenderer(ai.ownGrid)
    if(turn == 0)
      {
        println("Player begin the attack")
        val gameState = GameState(player,ai,"player")
        if(level==1)
          playerVsAiEasy(r,gameState)
        else if(level == 2)
          playerVsAiMedium(r,gameState,(List.empty[Position],""))
      }
    else
      {
        println("Ai begin the Attack")
        val gameState = GameState(ai,player,"ai")
        if(level==1)
          playerVsAiEasy(r,gameState)
        else if(level == 2)
          playerVsAiMedium(r,gameState,(List.empty[Position],""))
      }

    @tailrec
    def playerVsAiEasy(r:Random,gameState : GameState): Unit =
    {
      val nextTurn = GameHandlerUtil.nextTurn(gameState.turn)
      println("Attack phase !!"+gameState.turn+" procedding to attack")
      if(gameState.turn == "player")
        {
          println(gameState.turn+" grid : ")
          Render.playerGridRenderer(gameState.currentPlayer.ownGrid)
          println(gameState.turn+" enemy grid : ")
          Render.playerGridRenderer(gameState.currentPlayer.enemyGrid)

        }
      val positionShot = {
        if (gameState.turn == "player")
          PlayerInGameHandler.playerMakeAShot(gameState.currentPlayer)
        else
          AiInGameHandler.aiMakeAShot(gameState.currentPlayer,r)
      }
      if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == -1)
      {
        println("Miss Shot !!!!!")
        val newCurrentPlayer = Player.missShot(gameState.currentPlayer,positionShot)

        val newNextPlayer = Player.receiveMissShot(gameState.nextPlayer,positionShot)

        val newGameState = GameState(newNextPlayer,newCurrentPlayer,nextTurn)

        if(gameState.turn == "player")
          {

            Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
            println("Moving to defense phase ")
            println("****************")
            println("Entering "+nextTurn+" turn")
            StdIn.readLine("Press key to continue")
            println()
          }

        playerVsAiEasy(r,newGameState)
      }
      else if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == 0)
      {
        println("Shot Succeeded !!!!!")
        val newCurrentPlayer = Player.successShot(gameState.currentPlayer,positionShot)
        if(gameState.turn =="player")
          {
            Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
          }
        val newNextPlayer = Player.receiveSuccessShot(gameState.nextPlayer,positionShot)
        if(!Player.checkShipState(newNextPlayer,positionShot))
        {
          println("Ship Destroyed!!!!!")
          if(Player.checkFleetState(newNextPlayer.fleet))
          {
            val newGameState = GameState(newNextPlayer,newCurrentPlayer,nextTurn)
            if(gameState.turn == "player")
              {
                println("Moving to defense phase ")
                println("****************")
                println("Entering "+nextTurn+" turn")
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
          val newGameState = GameState(newNextPlayer,newCurrentPlayer,nextTurn)
          if(gameState.turn == "player")
            {
              println("Moving to defense phase ")
              println("****************")
              println("Entering "+nextTurn+" turn")
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
      val nextTurn = GameHandlerUtil.nextTurn(gameState.turn)
      println("Attack phase !!"+ gameState.turn+" procedding to attack")
      if(gameState.turn == "player")
      {
        println(gameState.turn+" grid : ")
        Render.playerGridRenderer(gameState.currentPlayer.ownGrid)
        println(gameState.turn+" enemy grid : ")
        Render.playerGridRenderer(gameState.currentPlayer.enemyGrid)

      }
      val positionShot = {
        if (gameState.turn == "player")
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
      println("*************"+positionShot)
      if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == -1)
      {
        println("Miss Shot !!!!!")

        val newCurrentPlayer = Player.missShot(gameState.currentPlayer,positionShot)

        val newNextPlayer = Player.receiveMissShot(gameState.nextPlayer,positionShot)

        val newGameState = GameState(newNextPlayer,newCurrentPlayer,nextTurn)

        if(gameState.turn == "player")
        {

          Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
          println("Moving to defense phase ")
          println("****************")
          println("Entering "+nextTurn+" turn")
          StdIn.readLine("Press key to continue")
          println()
        }
        if(gameState.turn == "player")
           playerVsAiMedium(r,newGameState,nextTarget)
        else
          {
            val newNextTarget = {
              if(nextTarget._1.size == 0)
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
        if(gameState.turn =="player")
        {
          Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
        }
        val newNextPlayer = Player.receiveSuccessShot(gameState.nextPlayer,positionShot)
        if(!Player.checkShipState(newNextPlayer,positionShot))
        {
          println("Ship Destroyed!!!!!")

          if(Player.checkFleetState(newNextPlayer.fleet))
          {
            val newGameState = GameState(newNextPlayer,newCurrentPlayer,nextTurn)
            if(gameState.turn == "player")
            {
              println("Moving to defense phase ")
              println("****************")
              println("Entering "+nextTurn+" turn")
              StdIn.readLine("Press key to continue")
              println()
            }
            if(gameState.turn == "ai")
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
          val newGameState = GameState(newNextPlayer,newCurrentPlayer,nextTurn)
          if(gameState.turn == "player")
          {
            println("Moving to defense phase ")
            println("****************")
            println("Entering "+nextTurn+" turn")
            println("Press any key to continue")
            StdIn.readLine()
            println()
            playerVsAiMedium(r,newGameState,nextTarget)
          }
          else if(gameState.turn == "ai")
            {
              val orientation = {
                if(nextTarget._1.size>0)
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
              val positionsToTarget = GameHandlerUtil.middleAiGetNextPosition(positionShot,orientation,gameState.currentPlayer.enemyGrid)
              val nextPositions = {
                if(nextTarget._1.isEmpty)
                  positionsToTarget
                else
                  nextTarget._1.tail ++ positionsToTarget
              }
              val newNextTarget = (nextPositions,orientation)
              println(positionShot +"********** "+newNextTarget._1+"*******"+orientation)
              playerVsAiMedium(r,newGameState,newNextTarget)
            }
        }
      }
    }

    @tailrec
    def playerVsAiHard(r:Random,gameState : GameState,nextTarget : List[(List[Position],String)])
    {
      val nextTurn = GameHandlerUtil.nextTurn(gameState.turn)
      println("Attack phase !!"+gameState.turn+" procedding to attack")
      if(gameState.turn == "player")
      {
        println(gameState.turn+" grid : ")
        Render.playerGridRenderer(gameState.currentPlayer.ownGrid)
        println(gameState.turn+" enemy grid : ")
        Render.playerGridRenderer(gameState.currentPlayer.enemyGrid)

      }
      val positionShot = {
        if (gameState.turn == "player")
          PlayerInGameHandler.playerMakeAShot(gameState.currentPlayer)
        else
          AiInGameHandler.aiMakeAShot(gameState.currentPlayer,r)
      }
      if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == -1)
      {
        println("Miss Shot !!!!!")
        val newCurrentPlayer = Player.missShot(gameState.currentPlayer,positionShot)

        val newNextPlayer = Player.receiveMissShot(gameState.nextPlayer,positionShot)

        val newGameState = GameState(newNextPlayer,newCurrentPlayer,nextTurn)

        if(gameState.turn == "player")
        {

          Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
          println("Moving to defense phase ")
          println("****************")
          println("Entering "+nextTurn+" turn")
          StdIn.readLine("Press key to continue")
          println()
        }

        playerVsAiHard(r,newGameState)
      }
      else if(gameState.nextPlayer.ownGrid.grid(positionShot.x)(positionShot.y) == 0)
      {
        println("Shot Succeeded !!!!!")
        val newCurrentPlayer = Player.successShot(gameState.currentPlayer,positionShot)
        if(gameState.turn =="player")
        {
          Render.playerGridRenderer(newCurrentPlayer.enemyGrid)
        }
        val newNextPlayer = Player.receiveSuccessShot(gameState.nextPlayer,positionShot)
        if(!Player.checkShipState(newNextPlayer,positionShot))
        {
          println("Ship Destroyed!!!!!")
          if(Player.checkFleetState(newNextPlayer.fleet))
          {
            val newGameState = GameState(newNextPlayer,newCurrentPlayer,nextTurn)
            if(gameState.turn == "player")
            {
              println("Moving to defense phase ")
              println("****************")
              println("Entering "+nextTurn+" turn")
              StdIn.readLine("Press key to continue")
              println()
            }

            playerVsAiHard(r,newGameState)
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
          val newGameState = GameState(newNextPlayer,newCurrentPlayer,nextTurn)
          if(gameState.turn == "player")
          {
            println("Moving to defense phase ")
            println("****************")
            println("Entering "+nextTurn+" turn")
            println("Press any key to continue")
            StdIn.readLine()
            println()
          }

          playerVsAiHard(r,newGameState)
        }
      }
    }

  }




}
