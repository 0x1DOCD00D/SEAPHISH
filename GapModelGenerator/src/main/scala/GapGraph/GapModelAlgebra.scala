package GapGraph

import Randomizer.UniformProbGenerator
import com.google.common.graph.*

import scala.collection.immutable.TreeSeqMap.OrderBy
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Random, Success, Try}

type GuiStateMachine = MutableValueGraph[GuiObject, Action]
class GapModel(val statesTotal: Int, val maxBranchingFactor: Int, val maxDepth: Int, val maxProperties: Int, val propValueRange:Int, val actionRange: Int, val seed:Option[Long] = None):
  require(statesTotal > 0, "The total number of states must be positive")
  require(maxBranchingFactor > 0, "The maximum branching factor must be greater than zero")
  require(maxDepth > 0, "The maximum depth must be greater than zero")
  require(maxProperties > 0, "The maximum number of properties must be greater than zero")
  require(propValueRange > 0, "The range of property values must be greater than zero")
  require(actionRange > 0, "The range of actions must be greater than zero")

  //noinspection UnstableApiUsage

  private val stateMachine: GuiStateMachine = ValueGraphBuilder.directed().build()
  private val (gen, offset, randValues) = UniformProbGenerator(UniformProbGenerator.createGenerator(seed),ints = true)

  private def createNodes(): Unit =
    (1 to statesTotal).foreach(id=>
      stateMachine.addNode(GuiObject(id, UniformProbGenerator.onDemand(gen, maxv = maxBranchingFactor),
        UniformProbGenerator.onDemand(gen, maxv = maxProperties), gen = gen, propValueRange = UniformProbGenerator.onDemand(gen, maxv = propValueRange),
        maxDepth = UniformProbGenerator.onDemand(gen, maxv = maxDepth), maxBranchingFactor = UniformProbGenerator.onDemand(gen, maxv = maxBranchingFactor),
        maxProperties = UniformProbGenerator.onDemand(gen, maxv = maxProperties)
        ))
      ()
    )

  private def createAction(from: GuiObject, to: GuiObject): Action =
    val fCount = from.childrenCount
    val tCount = to.childrenCount

    Action(UniformProbGenerator.onDemand(gen, maxv = actionRange),
      UniformProbGenerator.onDemand(gen, maxv = if fCount > 0 then fCount else 1),
      UniformProbGenerator.onDemand(gen, maxv = if fCount > 0 then fCount else 1),
      if UniformProbGenerator.onDemand(gen) % 2 == 0 then None else Some(UniformProbGenerator.onDemand(gen, maxv = propValueRange)),
      randValues.head.asInstanceOf[Double]
    )

  //  using this method we create a connected graph where there are no standalone unconnected nodes
  private def linkOrphanedNodesAndInitStates(allNodes: Array[GuiObject]): Unit =
    val orphans: Array[GuiObject] = allNodes.filter(node => stateMachine.incidentEdges(node).isEmpty)
    val connected: Array[GuiObject] = allNodes.filterNot(node => stateMachine.incidentEdges(node).isEmpty)
    orphans.foreach(node=>
      val other = connected(scala.util.Random.nextInt(connected.length))
      stateMachine.putEdgeValue(other, node, createAction(node, other))
    )

  private def addInitState(allNodes: Array[GuiObject], connectedness: Int): GuiObject =
    val maxOutdegree = stateMachine.nodes().asScala.map(node=>stateMachine.outDegree(node)).max
    val newInitNode:GuiObject = GuiObject(0, UniformProbGenerator.onDemand(gen, maxv = maxBranchingFactor),
      UniformProbGenerator.onDemand(gen, maxv = maxProperties), gen = gen, propValueRange = UniformProbGenerator.onDemand(gen, maxv = propValueRange),
      maxDepth = UniformProbGenerator.onDemand(gen, maxv = maxDepth), maxBranchingFactor = UniformProbGenerator.onDemand(gen, maxv = maxBranchingFactor),
      maxProperties = UniformProbGenerator.onDemand(gen, maxv = maxProperties)
    )
    stateMachine.addNode(newInitNode)
    val connected: Array[GuiObject] = allNodes.filter(node => stateMachine.outDegree(node) > (if maxOutdegree >= connectedness then connectedness else maxOutdegree - 1))
    connected.foreach(node=>
      stateMachine.putEdgeValue(newInitNode, node, createAction(newInitNode, node)))
    newInitNode

  def generateModel(upg:(UniformProbGenerator, Int), edgeProbability: Double = 0.3d): (GapGraph, UniformProbGenerator, Int) =
    createNodes()
    val allNodes: Array[GuiObject] = stateMachine.nodes().asScala.toArray
    val (gen, offset, probValues) = UniformProbGenerator(upg._1, upg._2, allNodes.length*allNodes.length)
    val pvIter: Iterator[Double] = probValues.iterator.asInstanceOf[Iterator[Double]]
    allNodes.foreach(node=>
      allNodes.foreach(other=>
        if node != other && pvIter.next() < edgeProbability then
          stateMachine.putEdgeValue(node, other, createAction(node, other))
        else ()
      )
    )
    linkOrphanedNodesAndInitStates(allNodes)
    (GapGraph(stateMachine, addInitState(allNodes,28)), gen, offset)
  end generateModel

object GapModelAlgebra:
//  each node of the graph is a GuiObject that corresponds to a GUI screen, which is a tree of GuiObjects
  trait GapGraphComponent()
