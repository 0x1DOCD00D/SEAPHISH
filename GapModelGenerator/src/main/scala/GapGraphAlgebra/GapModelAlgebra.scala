package GapGraphAlgebra

import GapGraphAlgebra.GapModelAlgebra.{actionRange, connectedness, edgeProbability, maxBranchingFactor, maxDepth, maxProperties, propValueRange, statesTotal}
import Randomizer.{SupplierOfRandomness, UniformProbGenerator}
import Utilz.ConfigReader.getConfigEntry
import Utilz.{CreateLogger, SPSConstants}
import Utilz.SPSConstants.{ACTIONRANGE, ACTIONRANGEDEFAULT, CONNECTEDNESS, CONNECTEDNESSDEFAULT, DEFAULTEDGEPROBABILITY, EDGEPROBABILITY, MAXBRANCHINGFACTOR, MAXBRANCHINGFACTORDEFAULT, MAXDEPTH, MAXDEPTHDEFAULT, MAXPROPERTIES, MAXPROPERTIESDEFAULT, PROPVALUERANGE, PROPVALUERANGEDEFAULT, SEED, STATESTOTAL, STATESTOTALDEFAULT}
import com.google.common.graph.*
import org.slf4j.Logger

import scala.collection.immutable.TreeSeqMap.OrderBy
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Random, Success, Try}

type GuiStateMachine = MutableValueGraph[GuiObject, Action]
class GapModel extends GapGraphConnectednessFinalizer:
  require(statesTotal > 0, "The total number of states must be positive")
  require(maxBranchingFactor > 0, "The maximum branching factor must be greater than zero")
  require(maxDepth > 0, "The maximum depth must be greater than zero")
  require(maxProperties > 0, "The maximum number of properties must be greater than zero")
  require(propValueRange > 0, "The range of property values must be greater than zero")
  require(actionRange > 0, "The range of actions must be greater than zero")

  //noinspection UnstableApiUsage

  private [this] val stateMachine: GuiStateMachine = ValueGraphBuilder.directed().build()

  private def createNodes(): Unit =
    (1 to statesTotal).foreach(id=>
      stateMachine.addNode(GuiObject(id, SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
        SupplierOfRandomness.onDemand(maxv = maxProperties), propValueRange = SupplierOfRandomness.onDemand(maxv = propValueRange),
        maxDepth = SupplierOfRandomness.onDemand(maxv = maxDepth), maxBranchingFactor = SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
        maxProperties = SupplierOfRandomness.onDemand(maxv = maxProperties)
        ))
      ()
    )

  private def createAction(from: GuiObject, to: GuiObject): Action =
    val fCount = from.childrenCount
    val tCount = to.childrenCount

    Action(SupplierOfRandomness.onDemand(maxv = actionRange),
      SupplierOfRandomness.onDemand(maxv = if fCount > 0 then fCount else 1),
      SupplierOfRandomness.onDemand(maxv = if fCount > 0 then fCount else 1),
      if SupplierOfRandomness.onDemand() % 2 == 0 then None else Some(SupplierOfRandomness.onDemand(maxv = propValueRange)),
      SupplierOfRandomness.randProbs(1).head
    )

  def generateModel(): GapGraph =
    createNodes()
    val allNodes: Array[GuiObject] = stateMachine.nodes().asScala.toArray
    val pvIter: Iterator[Double] = SupplierOfRandomness.randProbs(allNodes.length*allNodes.length).iterator
    allNodes.foreach(node=>
      allNodes.foreach(other=>
        if node != other && pvIter.next() < edgeProbability then
          stateMachine.putEdgeValue(node, other, createAction(node, other))
        else ()
      )
    )
    linkOrphanedNodesAndInitStates(allNodes)
    GapGraph(stateMachine, addInitState(allNodes))
  end generateModel

  def linkOrphanedNodesAndInitStates(allNodes: Array[GuiObject]): Unit =
    val orphans: Array[GuiObject] = allNodes.filter(node => stateMachine.incidentEdges(node).isEmpty)
    val connected: Array[GuiObject] = allNodes.filterNot(node => stateMachine.incidentEdges(node).isEmpty)
    orphans.foreach(node =>
      val other = connected(SupplierOfRandomness.onDemand(maxv = connected.length))
      Try(stateMachine.putEdgeValue(other, node, createAction(node, other))) match
        case Failure(exception) => GapModelAlgebra.logger.error(s"Failed to add an edge from $other to $node for reason ${exception.getMessage}")
        case Success(value) => ()
    )

  def addInitState(allNodes: Array[GuiObject]): GuiObject =
    val maxOutdegree = stateMachine.nodes().asScala.map(node => stateMachine.outDegree(node)).max
    val newInitNode: GuiObject = GuiObject(0, SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
      SupplierOfRandomness.onDemand(maxv = maxProperties), propValueRange = SupplierOfRandomness.onDemand(maxv = propValueRange),
      maxDepth = SupplierOfRandomness.onDemand(maxv = maxDepth), maxBranchingFactor = SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
      maxProperties = SupplierOfRandomness.onDemand(maxv = maxProperties)
    )
    stateMachine.addNode(newInitNode)
    val connected: Array[GuiObject] = allNodes.filter(node => stateMachine.outDegree(node) > (if maxOutdegree >= connectedness then connectedness else maxOutdegree - 1))
    connected.foreach(node =>
      stateMachine.putEdgeValue(newInitNode, node, createAction(newInitNode, node)))
    newInitNode

object GapModelAlgebra:
  val logger:Logger = CreateLogger(classOf[GapModel])

  val edgeProbability: Double = getConfigEntry(EDGEPROBABILITY, DEFAULTEDGEPROBABILITY)
  val statesTotal: Int = getConfigEntry(STATESTOTAL, STATESTOTALDEFAULT)
  val maxBranchingFactor: Int = getConfigEntry(MAXBRANCHINGFACTOR, MAXBRANCHINGFACTORDEFAULT)
  val maxDepth: Int = getConfigEntry(MAXDEPTH, MAXDEPTHDEFAULT)
  val maxProperties: Int = getConfigEntry(MAXPROPERTIES, MAXPROPERTIESDEFAULT)
  val propValueRange: Int = getConfigEntry(PROPVALUERANGE, PROPVALUERANGEDEFAULT)
  val actionRange: Int = getConfigEntry(ACTIONRANGE, ACTIONRANGEDEFAULT)
  val connectedness: Int = getConfigEntry(CONNECTEDNESS, CONNECTEDNESSDEFAULT)

  def apply(): GapGraph = new GapModel().generateModel()

//  each node of the graph is a GuiObject that corresponds to a GUI screen, which is a tree of GuiObjects
  @main def runGapModelAlgebra(args: String*): Unit =
    println("File /Users/drmark/Library/CloudStorage/OneDrive-UniversityofIllinoisChicago/Github/SeaPhish/GapModelGenerator/src/main/scala/GapGraph/GapModelAlgebra.scala created at time 5:42 PM")
