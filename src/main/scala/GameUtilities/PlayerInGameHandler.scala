package GameUtilities

import GameElement.{Grid, Player, Position, Ship}
import GameInterface.Render

import scala.annotation.tailrec
import scala.io.StdIn

/**
  * This class defines the function that will handle the interaction with the player as a user
  */
object PlayerInGameHandler {



  /**
    * Convert a string to an int
    * @param s : the string to convert
    * @return : Option[Int]. If the convertion fails return None
    */
  def toInt(s : String) : Option[Int] =
  {
    try
    {
      Some(s.toInt)
    }
    catch
      {
        case e : Exception => None
      }
  }

  /**
    * seize the line on which the ship will be placed
    * @return : the line number included between 0 and 9
    */
  @tailrec
  def seizeLineNumberShip() : Int =
  {
    println("Choose the Line number : ")
    val x = StdIn.readLine()
    if(toInt(x) == None)
    {
      println("Input Error !!!! You have to seize a number")
      seizeLineNumberShip()
    }
    else
    {
      if(toInt(x).get <1 || toInt(x).get >10)
        {
          println("Error Input !!! you must seize a number between 1 and 10")
          seizeLineNumberShip()
        }
      else
        {
          toInt(x).get - 1
        }
    }
  }

  /**
    * Seize the column on which the ship will be seized
    * @return the column number
    */
  @tailrec
  def seizeColumnNumberShip():Int =
  {
    println("Choose column : ")
    println("(A,B,C,D,E,F,G,H,I,J)")
    val column = StdIn.readLine().toUpperCase()
    column match
    {
      case "A" => 0
      case "B" => 1
      case "C" => 2
      case "D" => 3
      case "E" => 4
      case "F" => 5
      case "G" => 6
      case "H" => 7
      case "I" => 8
      case "J" => 9
      case _ => {
        println("Input Error!!!! you have to choose between A,B,C,D,E,F,G,H,I or J")
        seizeColumnNumberShip()
      }
    }

  }

  /**
    * seize the orientation of the ship, will it be placed horizontally (right or left) or vertically(up or down)
    * @return
    */
  @tailrec
  def seizeOrientationShip() : String =
  {
    println("Which orientation you want for the ship? (H for horizontal , V for vertical")
    val orientation = StdIn.readLine().toUpperCase()
    if(orientation == "H" )
      {
        println("In which way ? to the right or the the left (type R for right, L for left")
        val way = StdIn.readLine().toUpperCase
        if(way == "L" || way =="R")
          {
            orientation+way
          }
        else
          {
            println("You have to type r for right or l for left")
            seizeOrientationShip()
          }

      }
    else if(orientation == "V")
      {
        println("In which way ? up or down? U for up, D for down")
        val way = StdIn.readLine().toUpperCase
        if(way == "U" || way =="D")
        {
          orientation+way
        }
        else
        {
          println("You have to type U for up or D for Down")
          seizeOrientationShip()
        }
      }
    else
      {
        println("Wrong input !!! please type h for horizontal or v for vertical")
        seizeOrientationShip()
      }
  }

  /**
    * This function will generate the ship with its corresponding positions
    * @param shipSize : the shize of the ship to generate
    * @param player : the player who places the ship
    * @return : a tuple of list of positions and the ship
    */
  @tailrec
  def generateShipWithPosition(shipSize : Int, player : Player) : (List[Position],Ship) =
  {
    val ship = Ship(true,shipSize)
    val line = seizeLineNumberShip()
    val column = seizeColumnNumberShip()
    val orientation = seizeOrientationShip()
    if(GameHandlerUtil.positionLimitsCheck(Position(line,column),orientation,shipSize))
      {
        val positions = GameHandlerUtil.generateShipPosition(orientation,shipSize,Position(line,column))
        if(!Player.occupiedPosition(player,positions))
        {
          (positions,ship)
        }
        else
        {
          println("Ships overlapping !!!!Choose another position for this ship")
          generateShipWithPosition(shipSize,player)
        }
      }
    else
      {
        println("Ship position out of limits !!!! please enter another position")
        generateShipWithPosition(shipSize,player)
      }

  }

  /**
    * generate all player's ships with their respective positions
    * @param shipsClass : the list of ships that can be added to a game
    * @return : player with all of his ships and his grid updated
    */
  def generatePlayerWithItsShip(shipsClass : List[(String,Int)],playerType : String) : Player =
  {

    @tailrec
    def generatePlayerWithItsShipTailRec(player : Player, index : Int) : Player =
    {
      if(index == shipsClass.size)
        player
      else
        {
          println("Positioning of : "+shipsClass(index)._1 +" ("+shipsClass(index)._2+" squares)")
          val shipSize = shipsClass(index)._2
          val shipWithPositions = generateShipWithPosition(shipSize,player)
          val playerWithShipAdded = Player.addShip(player,shipWithPositions)
          Render.playerGridRenderer(playerWithShipAdded.ownGrid)
          generatePlayerWithItsShipTailRec(playerWithShipAdded,index+1)
        }
    }
    val player = Player(List.empty[(List[Position],Ship)],Grid(),Grid(),playerType)
    Render.playerGridRenderer(player.ownGrid)
    generatePlayerWithItsShipTailRec(player,0)
  }



  /**
    *
    * @param player
    * @return
    */
  def playerMakeAShot(player : Player):Position =
  {
    val line = seizeLineNumberShip()
    val column = seizeColumnNumberShip()
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
}