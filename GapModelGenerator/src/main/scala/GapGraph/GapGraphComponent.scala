package GapGraph

import Randomizer.UniformProbGenerator

trait GapGraphComponent

case class GuiObject(id: Int, children: Int, props: Int, currentDepth: Int = 1,
                     gen:UniformProbGenerator, propValueRange:Int, maxDepth:Int, maxBranchingFactor:Int, maxProperties:Int ) extends GapGraphComponent:
  val properties: List[Int] = List.fill(props)(UniformProbGenerator.onDemand(gen, maxv = propValueRange))
  val childrenObjects: List[GuiObject] = if currentDepth <= maxDepth then
    List.tabulate(children)(cid => GuiObject(cid,
      UniformProbGenerator.onDemand(gen, maxv=maxBranchingFactor),
      UniformProbGenerator.onDemand(gen, maxv=maxProperties), currentDepth + 1, gen,
      propValueRange, maxDepth, maxBranchingFactor, maxProperties))
  else List.empty

  def childrenCount: Int = children + childrenObjects.map(_.childrenCount).sum

case class Action(actionType: Int, fromId: Int, toId: Int, resultingValue: Option[Int], cost: Double) extends GapGraphComponent
