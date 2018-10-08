package GameElementTest

import org.scalatest.{FlatSpec, Matchers}
import GameElement.{Grid, Player, Position, Ship}

/**
  * This class is to test the Player functionalities
  * It will test the update of the different grids when shots are made
  * It will test if the player check correctly the state of his ship
  * It will test if the ships are correctly added on the player's grid
  */
class PlayerTest extends FlatSpec with Matchers {

  val player = Player(List.empty[(List[Position],Ship)],Grid(),Grid(),"player")

  val fleet = (List.apply(Position(1,2),Position(1,3),Position(1,4)),Ship(true,3))

  /**
    * Testing that the ship is added correctly on the grid
    * the element of the grid at the position of one of the ship's square is equal to 0
    */
  "Player" should "add his ships on his grid on their respective position" in
  {
    val playerWithAShip = Player.addShip(player,fleet)
    playerWithAShip.fleet.size > 0 should be (true)
    playerWithAShip.ownGrid.grid(1)(2) should be (0)
    playerWithAShip.ownGrid.grid(1)(3) should be (0)
    playerWithAShip.ownGrid.grid(1)(4) should be (0)
  }

  /**
    * testing if a square or a list of square are occupied
    */
  "Player" should "check if position on his grid are occupied" in
  {
    Player.occupiedPosition(player,fleet._1) should be (false)
    val playerWithAShip = Player.addShip(player,fleet)
    Player.occupiedPosition(playerWithAShip,fleet._1) should be (true)
    Player.occupiedPosition(playerWithAShip,List.apply(Position(1,2))) should be (true)
  }

  /**
    * test if the player updates correctly his grid when he receives a miss shot
    */
  "Player" should "update his grid when he receives a miss shot" in
  {
    val playerUpdatedGrid = Player.receiveMissShot(player,Position(2,2))
    playerUpdatedGrid.ownGrid.grid(2)(2) should be (1)
  }

  /**
    * test if the player updates correctly his gird when he receives a successful shot
    */
  "Player" should "update his grid when he received a success shot" in
  {
    val playerWithAShip = Player.addShip(player,fleet)
    val playerUpdatedGrid = Player.receiveSuccessShot(playerWithAShip,Position(1,4))

    playerUpdatedGrid.ownGrid.grid(1)(4) should be (2)
    val touchedShip = playerUpdatedGrid.fleet.filter(x => x._1.contains(Position(1,4)))(0)._2
    touchedShip.squareNumber should be (2)
  }

  /**
    * test if the player update correctly his enemy grid when he makes a muss shot
    */
  "Player" should "update the enemy grid when he makes a miss shot" in
  {
    val playerUpdatedGrid = Player.missShot(player,Position(2,2))
    playerUpdatedGrid.enemyGrid.grid(2)(2) should be (1)
  }

  /**
    * test if the player updates correctly his enemy grid when he makes a successful shot
    */
  "Player" should "update the enemy grid when he makes a success shot" in
  {
    val playerUpdatedGrid = Player.successShot(player,Position(2,2))
    playerUpdatedGrid.enemyGrid.grid(2)(2) should be (2)
  }

  /**
    * test if the player checks correctly the state of his ship when this one get touched on one of his square
    */
  "Player" should "Check if his ship is still operational when it has been damaged" in
  {
    val playerWithAShip = Player.addShip(player,fleet)
    val shipTouched1 = Player.receiveSuccessShot(playerWithAShip,Position(1,2))
    Player.checkShipState(shipTouched1,Position(1,2)) should be (true)
    val shipTouched2 = Player.receiveSuccessShot(shipTouched1,Position(1,3))
    Player.checkShipState(shipTouched2,Position(1,3)) should be (true)
    val shipTouched3 = Player.receiveSuccessShot(shipTouched2,Position(1,4))
    Player.checkShipState(shipTouched3,Position(1,4)) should be (false)
  }

  "Player" should "Check if he still have operational ships" in
  {
    val ship1 = (List.apply(Position(1,2)),Ship(true,3))
    val ship2 = (List.apply(Position(1,2)),Ship(false,3))
    val ship3 = (List.apply(Position(1,2)),Ship(true,3))
    val ship4 = (List.apply(Position(1,2)),Ship(false,3))
    val fleet1 = List.apply(ship1,ship2,ship3)
    var destroyedFleet = List.apply(ship2,ship4)
    val playerWithAFleet= Player.addShip(player,fleet)
    val playerWithFleetDestroyed = Player(destroyedFleet,Grid(),Grid(),"player")
    Player.checkFleetState(playerWithAFleet.fleet) should be (true)
    Player.checkFleetState(playerWithFleetDestroyed.fleet) should be (false)
  }

}
