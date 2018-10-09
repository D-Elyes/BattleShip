package GameUtilitiesTest

import GameElement.{Grid, Position}
import GameUtilities.{GeneralActionsForAi, MediumAiActions}
import org.scalatest.{FlatSpec, Matchers}

class MediumAiActionsTest  extends FlatSpec with Matchers  {

  "Medium Ai" should "the next positions that it will target" in
    {
      val grid = Grid()
      val p = Position(3,3)
      val orientation = "V"
      val nextPositions = MediumAiActions.mediumAiGetNextTargets(p,orientation,grid)

      nextPositions(0) should be (Position(4,3))
      nextPositions(1) should be (Position(2,3))
    }

}
