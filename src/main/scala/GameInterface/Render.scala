package GameInterface

import GameElement.{Grid}

object Render {

  def menuRederer() =
  {
    println("1- Player vs Player")
    println("2- Player Vs AI")
    println("3- Ai vs AI")
    println("4- Quit game")


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
            case 0 => print(Console.GREEN_B +"S "+Console.RESET)
            case 1 => print("X ")
            case 2 => print(Console.RED_B +"S "+Console.RESET)
          }
      })
      println()
    })
    println()
  }

  def gameClose() =
  {
    println("Thank you for playing Battle Ship !!!!")
  }

  def aiLevelChoice(): Unit =
  {
    println("1- Easy")
    println("2- Normal")
    println("3- Hard")
  }

}
