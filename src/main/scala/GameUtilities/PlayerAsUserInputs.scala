package GameUtilities

import scala.annotation.tailrec
import scala.io.StdIn

/**
  * This class will manage the users inputs when he begins the game.
  * This will handle the inputs of the ships positions and their orientations
  */
object PlayerAsUserInputs {

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
    //Check if the user entered a number
    if(toInt(x) == None)
    {
      println("Input Error !!!! You have to seize a number")
      seizeLineNumberShip()
    }
    else
    {
      //The user must enter a number between 1 and 10 as it is the dimensions of the grids
      if(toInt(x).get <1 || toInt(x).get >10)
      {
        //if the number isn't valid, an error message will be displayed and the user will be asked to enter another number
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
    * Seize the column on which the ship will be placed
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
}
