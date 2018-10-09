package GameUtilitiesTest

import GameElement.{Position}
import org.scalatest.{FlatSpec, Matchers}
import GameUtilities.{ GeneralGameManagement}

class GeneralGameManagementTest extends FlatSpec with Matchers {

  "Handling a player" should "mean generation the ship positions according a position he entered and an orientation" in
  {
    val positions = GeneralGameManagement.generateShipPosition("HR",5,Position(0,0))
    positions.size should be (5)
    positions(0) should be (Position(0,0))
    positions(1) should be (Position(0,1))
    positions(2) should be (Position(0,2))
    positions(3) should be (Position(0,3))
    positions(4) should be (Position(0,4))

    val positionsHL = GeneralGameManagement.generateShipPosition("HL",4,Position(0,9))
    positionsHL.size should be (4)
    positionsHL(0) should be (Position(0,9))
    positionsHL(1) should be (Position(0,8))
    positionsHL(2) should be (Position(0,7))
    positionsHL(3) should be (Position(0,6))

    val positionsVU = GeneralGameManagement.generateShipPosition("VU",3,Position(5,9))
    positionsVU.size should be (3)
    positionsVU(0) should be (Position(5,9))
    positionsVU(1) should be (Position(4,9))
    positionsVU(2) should be (Position(3,9))

    val positionsVD = GeneralGameManagement.generateShipPosition("VD",2,Position(5,9))
    positionsVD.size should be (2)
    positionsVD(0) should be (Position(5,9))
    positionsVD(1) should be (Position(6,9))
  }



}
