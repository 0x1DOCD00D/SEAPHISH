package GapGraph

object GapModelAlgebra:
  trait GapGraphComponent
  /*
  case class GuiObject(id: Int, children: Int, props: Int, currentDepth: Int = 1) extends GapGraphComponent:
    val properties: List[Int] = List.fill(props)(scala.util.Random.nextInt(propValueRange))
    val childrenObjects: List[GuiObject] = if currentDepth <= maxDepth then
      List.tabulate(children)(cid => GuiObject(cid, scala.util.Random.nextInt(maxBranchingFactor), scala.util.Random.nextInt(maxProperties), currentDepth + 1))
    else List.empty

    def childrenCount: Int = children + childrenObjects.map(_.childrenCount).sum

  case class Action(actionType: Int, fromId: Int, toId: Int, resultingValue: Option[Int], cost: Double) extends GapGraphComponent
*/