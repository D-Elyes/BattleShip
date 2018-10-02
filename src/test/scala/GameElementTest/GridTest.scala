package GameElementTest

import GameElement.{Grid, Position}
import org.scalatest.{FlatSpec, Matchers}

/**
  * This class is for testing the Grid class
  */
class GridTest extends  FlatSpec with Matchers{
  val grid : Grid = Grid()

  /**
    * Heer we test the initialization of the grid
    */
  "Grid" should "Have all its element equal to -1 " in
  {
    grid.grid.flatMap(x => x).foreach(x => x should be (-1))
  }

  /**
    * Here is the test of an update of an element of the Grid
    */
  "Grid" should "update its element" in
  {
    val newGrid = Grid.updateGrid(grid,Position(1,1),2)
    newGrid.grid(1)(1) should be (2)
  }

}
