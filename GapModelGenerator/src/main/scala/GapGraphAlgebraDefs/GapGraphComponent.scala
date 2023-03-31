package GapGraphAlgebraDefs

import Randomizer.{SupplierOfRandomness, UniformProbGenerator}
import Randomizer.UniformProbGenerator.*
trait GapGraphComponent

case class GuiObject(id: Int, children: Int, props: Int, currentDepth: Int = 1, propValueRange:Int, maxDepth:Int, maxBranchingFactor:Int, maxProperties:Int, modifiedProperties: List[Int] = List(), modifiedChildren:List[GuiObject] = List() ) extends GapGraphComponent:
  val properties: List[Int] = if modifiedProperties.nonEmpty then modifiedProperties else List.fill(props)(SupplierOfRandomness.onDemand(maxv = propValueRange))
  val childrenObjects: List[GuiObject] = if modifiedChildren.nonEmpty then modifiedChildren else
    if currentDepth <= maxDepth then
      List.tabulate(children)(cid => GuiObject(cid+id+1,
        SupplierOfRandomness.onDemand(maxv=maxBranchingFactor),
        SupplierOfRandomness.onDemand(maxv=maxProperties), currentDepth + 1,
        propValueRange, maxDepth, maxBranchingFactor, maxProperties))
    else List.empty

  def childrenCount: Int = children + childrenObjects.map(_.childrenCount).sum


  def modify: GuiObject =
    val probs: Iterator[Double] = SupplierOfRandomness.randProbs(properties.length +  childrenObjects.length).iterator
    val modifiedProperties: List[Int] = properties.map(p => if probs.next() <= GapModelAlgebra.modificationProbability then SupplierOfRandomness.onDemand(maxv = propValueRange) else p)
    val modChildrenObjects: List[GuiObject] = childrenObjects.flatMap( co =>
      if probs.next() < GapModelAlgebra.modificationProbability then
          if SupplierOfRandomness.`YesOrNo?` then
            GuiObject(co.id,
              if SupplierOfRandomness.`YesOrNo?` then SupplierOfRandomness.onDemand(maxv = maxBranchingFactor) else co.maxBranchingFactor,
              if SupplierOfRandomness.`YesOrNo?` then SupplierOfRandomness.onDemand(maxv = maxProperties) else co.maxProperties,
              currentDepth,
              co.propValueRange, co.maxDepth, co.maxBranchingFactor, co.maxProperties) :: Nil
          else co.modify :: Nil
      else co :: Nil)
    GuiObject(id, children, props, currentDepth, propValueRange, maxDepth, maxBranchingFactor, maxProperties, modifiedProperties, modChildrenObjects)


case class Action(actionType: Int, fromId: Int, toId: Int, resultingValue: Option[Int], cost: Double) extends GapGraphComponent
