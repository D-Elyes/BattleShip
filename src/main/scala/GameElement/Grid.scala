package GameElement

import scala.collection.mutable.ListBuffer

/**
  * This is the grid of the game
  *
  * @param grid : It is a list of list of Intger, a square is represented by one element of this list.
  *             Each element contains a number that has an interpretation :
  *             -1 : the square contains nothing
  *             0 : this is placement of one of the ship's  square
  *             1 : a player shots on this square but missed the ship
  *             2 : a player shots on this square and touched a ship
  */
case class Grid(grid : List[List[Int]])

object Grid
{
  /**
    * Initialize a grid. As a grid contains nothing at the beginning all of its elements contains -1
    * @param grid : the grid we want to initilize
    */
  def initilizingGrid() : List[List[Int]]=
  {
    List.fill(10)(List.fill(10)(-1))
  }


  /**
    * This function update a grid by making a copy of it and changing the element at a given position
    * @param oldGrid : the grid to update
    * @param squarePosition : the position of the element to update
    * @param newSquareState : the new value of the element to be changed
    * @return : a new grid with the value of the element updated
    */
  def updateGrid(oldGrid : Grid, squarePosition:Position, newSquareState : Int) : Grid =
  {

    val newGrid = oldGrid.grid.updated(squarePosition.x,oldGrid.grid(squarePosition.x).updated(squarePosition.y,newSquareState))
    Grid(newGrid)

  }







}
