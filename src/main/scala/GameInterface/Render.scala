package GameInterface

import GameElement.{Grid}

/**
  * This class will contain functions that will display different information to the user
  * like the main menu, the gird, the close game message, etc..
  */
object Render {

  /**
    * Render the main menu of the game
    */
  def menuRederer()
  {
    println("1- Player vs Player")
    println("2- Player Vs AI")
    println("3- Ai vs AI")
    println("4- Quit game")
  }

  /**
    * Render a grid
    * @param playerGrid : the player's grid that will be displayer
    */
  def playerGridRenderer(playerGrid : Grid)
  {
    println("\tA B C D E F G H I J")
    playerGrid.grid.zipWithIndex.foreach(x => {
      print((x._2+1)+"\t")
      x._1.foreach(y => {
          y match
          {
            case -1 => print("_ ")
            case 0 => print(Console.GREEN_B +"S "+Console.RESET)
            case 1 => print("X ")
            case 2 => print(Console.RED_B +"S "+Console.RESET)
          }
      })
      println()
    })
    println()
  }

  /**
    * Display a message when the user quits the game
    */
  def gameClose()
  {
    println("Thank you for playing Battle Ship !!!!")
  }

  /**
    * Display the menu of the level's Ai choice
    */
  def aiLevelChoice(): Unit =
  {
    println("1- Easy")
    println("2- Normal")
    println("3- Hard")
  }
}
