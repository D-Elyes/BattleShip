package GameElementTest

import GameElement.Ship
import org.scalatest.{FlatSpec, Matchers}

/**
  * This class is made to test the functionalities of the ship class
  */
class ShipTest extends FlatSpec with Matchers{
  val shipSize : Int = 5
  val ship = Ship(true,shipSize) //the ship on which we will do our tests

  /**
    *Testing a ship that get damaged
    */
  "Ship" should "have less squares when damaged" in
    {
      val damagedShip = Ship.shipDamaged(ship)
      damagedShip.squareNumber  should be (4)
    }


  /**
    * testing a ship that get destroyed
    */
  "Ship" should "be not operational when destroyed" in
  {
    val shipDestroyed = Ship.shipDestroyed(ship)
    shipDestroyed.operational should be (false)
  }




}
