package GapGraphAlgebraDefs

import Randomizer.{SupplierOfRandomness, UniformProbGenerator}
import Randomizer.UniformProbGenerator.*
trait GapGraphComponent

case class GuiObject(id: Int, children: Int, props: Int, currentDepth: Int = 1, propValueRange:Int, maxDepth:Int, maxBranchingFactor:Int, maxProperties:Int) extends GapGraphComponent:
  val properties: List[Int] = List.fill(props)(SupplierOfRandomness.onDemand(maxv = propValueRange))
  val childrenObjects: List[GuiObject] =
    if currentDepth <= maxDepth then
      List.tabulate(children)(cid => GuiObject(cid+id+1,
        SupplierOfRandomness.onDemand(maxv=maxBranchingFactor),
        SupplierOfRandomness.onDemand(maxv=maxProperties), currentDepth + 1,
        propValueRange, maxDepth, maxBranchingFactor, maxProperties))
    else List.empty
  def childrenCount: Int = children + childrenObjects.map(_.childrenCount).sum
  def modify: GuiObject =
    GuiObject(id,
      children,
      props,
      currentDepth,
      SupplierOfRandomness.onDemand(maxv = propValueRange),
      maxDepth,
      SupplierOfRandomness.onDemand(maxv = propValueRange),
      maxProperties
    )

case class Action(actionType: Int, fromId: Int, toId: Int, resultingValue: Option[Int], cost: Double) extends GapGraphComponent:
  def modify: Action = Action(SupplierOfRandomness.onDemand(maxv = GapModelAlgebra.actionRange), fromId, toId, resultingValue, SupplierOfRandomness.randProbs(1).head)

case object TerminalNode extends GapGraphComponent
case object TerminalAction extends GapGraphComponent