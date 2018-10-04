package GameInterface

import GameElement.{Grid}

object Render {

  def menuRederer() =
  {
    println("Welcome to Battle Ship\n\n")
    println("You can choose game mode : ")
    println("1- Player vs Player")
    println("2- Player Vs AI")
    println("3- Ai vs AI")
    println("4- Quit game")
    println("Please type the number of the option(1,2,3 or 4 )")

  }

  def playerGridRenderer(playerGrid : Grid) =
  {
    println("\tA B C D E F G H I J")
    playerGrid.grid.zipWithIndex.foreach(x => {
      print((x._2+1)+"\t")
      x._1.foreach(y => {

          y match
          {
            case -1 => print("_ ")
            case 0 => print(Console.GREEN +"S "+Console.WHITE)
            case 1 => print("X ")
            case 2 => print(Console.RED +"S " +Console.WHITE)
          }
      })
      println()
    })
    println()
  }

  def gameClose() =
  {
    
  }



}
