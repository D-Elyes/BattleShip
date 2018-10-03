package GameUtil

import GameElement.Position

import scala.annotation.tailrec
import scala.io.StdIn

object PlayerInGameHandler {

  /**
    * Generate the list of positions of a ship starting with a position the the user typed and according to an orientation
    * @param orientation : the orientation of the ship :
    *                    HR : Horizontal Right
    *                    HL : Horizontal Left
    *                    VU : Vertical Up
    *                    VD : Vertical Down
    * @param shipSize : The size of the ship to be place
    * @param p : the position that the player choose
    * @return : a list of Position which the first element is the one that the player seized
    */
  def generateShipPosition(orientation: String, shipSize: Int, p: Position): List[Position] = {
    if (shipSize == 0)
      Nil
    else {
      orientation match {
        case "HR" => p :: generateShipPosition(orientation, shipSize - 1, Position(p.x, p.y + 1))
        case "HL" => p :: generateShipPosition(orientation, shipSize - 1, Position(p.x, p.y - 1))
        case "VU" => p :: generateShipPosition(orientation, shipSize - 1, Position(p.x - 1, p.y))
        case "VD" => p :: generateShipPosition(orientation, shipSize - 1, Position(p.x + 1, p.y))
      }
    }
  }

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
    println("On which line you want to place the ship")
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
  def seizeColomnNumberShip():Int =
  {
    println("On which column you want to place your ship?")
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
        seizeColomnNumberShip()
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



}