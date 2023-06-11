package GapGraphAlgebraDefs

import GapGraphAlgebraDefs.GapModelAlgebra.{actionRange, connectedness, createAction, edgeProbability, logger, maxBranchingFactor, maxDepth, maxProperties, propValueRange, statesTotal}
import Randomizer.{SupplierOfRandomness, UniformProbGenerator}
import Utilz.ConfigReader.getConfigEntry
import Utilz.{CreateLogger, SPSConstants}
import Utilz.SPSConstants.{ACTIONRANGE, ACTIONRANGEDEFAULT, CONNECTEDNESS, CONNECTEDNESSDEFAULT, COSTOFDETECTION, COSTOFDETECTIONDEFAULT, DEFAULTDISTANCECOEFFICIENT, DEFAULTDISTANCESPREADTHRESHOLD, DEFAULTEDGEPROBABILITY, DEFAULTMODIFICATIONPROBABILITY, DEFAULTPERTURBATIONCOEFFICIENT, DISTANCECOEFFICIENT, DISTANCESPREADTHRESHOLD, DOPPLEGANGERS, DOPPLEGANGERSDEFAULT, EDGEPROBABILITY, GRAPHWALKNODETERMINATIONPROBABILITY, GRAPHWALKNODETERMINATIONPROBABILITYDEFAULT, GRAPHWALKTERMINATIONPOLICY, GRAPHWALKTERMINATIONPOLICYDEFAULT, MALAPPBUDGET, MALAPPBUDGETDEFAULT, MAXBRANCHINGFACTOR, MAXBRANCHINGFACTORDEFAULT, MAXDEPTH, MAXDEPTHDEFAULT, MAXPROPERTIES, MAXPROPERTIESDEFAULT, MAXWALKPATHLENGTHCOEFF, MAXWALKPATHLENGTHCOEFFDEFAULT, MODIFICATIONPROBABILITY, PERTURBATIONCOEFFICIENT, PROPVALUERANGE, PROPVALUERANGEDEFAULT, SEED, SERVICEPENALTY, SERVICEPENALTYDEFAULT, SERVICEREWARD, SERVICEREWARDDEFAULT, SERVICEREWARDPROBABILITY, SERVICEREWARDPROBABILITYDEFAULT, STATESTOTAL, STATESTOTALDEFAULT, TARGETAPPHIGHPENALTY, TARGETAPPHIGHPENALTYDEFAULT, TARGETAPPLOWPENALTY, TARGETAPPLOWPENALTYDEFAULT, TARGETAPPSCORE, TARGETAPPSCOREDEFAULT, WALKS, WALKSDEFAULT}
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

  private [this] val stateMachine: GuiStateMachine = ValueGraphBuilder.directed().build()
  val modelUUID:String = java.util.UUID.randomUUID.toString

  private def createNodes(): Unit =
    (1 to statesTotal).foreach(id=>
      if !stateMachine.addNode(GuiObject(id, SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
        SupplierOfRandomness.onDemand(maxv = maxProperties), propValueRange = SupplierOfRandomness.onDemand(maxv = propValueRange),
        maxDepth = SupplierOfRandomness.onDemand(maxv = maxDepth), maxBranchingFactor = SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
        maxProperties = SupplierOfRandomness.onDemand(maxv = maxProperties)
        )) then logger.error(s"Could not add node with id $id")
      ()
    )

  def generateModel(forceLinkOrphans: Boolean = false): GapGraph =
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
    val initState: GuiObject = addInitState(allNodes)
    val generatedGraph: GapGraph = GapGraph(stateMachine, initState)
    if forceLinkOrphans then
      val unreachableNodes: Set[GuiObject] = generatedGraph.unreachableNodes()._1
      val reachableNodes: Set[GuiObject] = stateMachine.nodes().asScala.toSet -- unreachableNodes
      val rnSize = reachableNodes.size
      if rnSize > 0 then
        val arrReachableNodes = reachableNodes.toArray
        unreachableNodes.foreach(urn =>
          val index = SupplierOfRandomness.onDemand(maxv = rnSize)
          stateMachine.putEdgeValue(arrReachableNodes(index), urn, createAction(arrReachableNodes(index), urn))
        )
      else unreachableNodes.foreach(unreachableNode =>stateMachine.putEdgeValue(initState, unreachableNode, createAction(initState, unreachableNode)))
    generatedGraph
  end generateModel

  def addInitState(allNodes: Array[GuiObject]): GuiObject =
    val maxOutdegree = stateMachine.nodes().asScala.map(node => stateMachine.outDegree(node)).max
    val newInitNode: GuiObject = GuiObject(0, SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
      SupplierOfRandomness.onDemand(maxv = maxProperties), propValueRange = SupplierOfRandomness.onDemand(maxv = propValueRange),
      maxDepth = SupplierOfRandomness.onDemand(maxv = maxDepth), maxBranchingFactor = SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
      maxProperties = SupplierOfRandomness.onDemand(maxv = maxProperties)
    )
    stateMachine.addNode(newInitNode)
    val orphans: Array[GuiObject] = allNodes.filter(node =>
//      GapModelAlgebra.logger.info(s"${stateMachine.inDegree(node)}")
      stateMachine.incidentEdges(node).isEmpty)
    orphans.foreach(node =>
      stateMachine.putEdgeValue(newInitNode, node, createAction(newInitNode, node)))
    val connected: Array[GuiObject] = allNodes.filter(node => stateMachine.outDegree(node) > (if maxOutdegree >= connectedness then connectedness else maxOutdegree - 1))
    connected.foreach(node =>
      stateMachine.putEdgeValue(newInitNode, node, createAction(newInitNode, node)))
    newInitNode

object GapModelAlgebra:
  val logger:Logger = CreateLogger(classOf[GapModel])

  val dopplegangers: Int = getConfigEntry(SPSConstants.configGapModel, DOPPLEGANGERS, DOPPLEGANGERSDEFAULT)
  val distanceSpreadThreshold: Double = getConfigEntry(SPSConstants.configGapModel, DISTANCESPREADTHRESHOLD, DEFAULTDISTANCESPREADTHRESHOLD)
  val perturbationCoeff: Double = getConfigEntry(SPSConstants.configGapModel, PERTURBATIONCOEFFICIENT, DEFAULTPERTURBATIONCOEFFICIENT)
  val distanceCoeff: Double = getConfigEntry(SPSConstants.configGapModel, DISTANCECOEFFICIENT, DEFAULTDISTANCECOEFFICIENT)
  val edgeProbability: Double = getConfigEntry(SPSConstants.configGapModel,EDGEPROBABILITY, DEFAULTEDGEPROBABILITY)
  val guiObjectModificationProbability: Double = getConfigEntry(SPSConstants.configGapModel,MODIFICATIONPROBABILITY, DEFAULTMODIFICATIONPROBABILITY)
  val statesTotal: Int = getConfigEntry(SPSConstants.configGapModel,STATESTOTAL, STATESTOTALDEFAULT)
  val numberOfWalks: Int = getConfigEntry(SPSConstants.configGapModel,WALKS, WALKSDEFAULT)
  val maxBranchingFactor: Int = getConfigEntry(SPSConstants.configGapModel,MAXBRANCHINGFACTOR, MAXBRANCHINGFACTORDEFAULT)
  val maxDepth: Int = getConfigEntry(SPSConstants.configGapModel,MAXDEPTH, MAXDEPTHDEFAULT)
  val maxProperties: Int = getConfigEntry(SPSConstants.configGapModel,MAXPROPERTIES, MAXPROPERTIESDEFAULT)
  val propValueRange: Int = getConfigEntry(SPSConstants.configGapModel,PROPVALUERANGE, PROPVALUERANGEDEFAULT)
  val actionRange: Int = getConfigEntry(SPSConstants.configGapModel,ACTIONRANGE, ACTIONRANGEDEFAULT)
  val connectedness: Int = getConfigEntry(SPSConstants.configGapModel,CONNECTEDNESS, CONNECTEDNESSDEFAULT)
  val maxWalkPathLengthCoeff: Double = getConfigEntry(SPSConstants.configGapModel,MAXWALKPATHLENGTHCOEFF, MAXWALKPATHLENGTHCOEFFDEFAULT)
  val graphWalkTerminationPolicy: String = getConfigEntry(SPSConstants.configGapModel,GRAPHWALKTERMINATIONPOLICY, GRAPHWALKTERMINATIONPOLICYDEFAULT)
  val graphWalkNodeTerminationProbability: Double = getConfigEntry(SPSConstants.configGapModel,GRAPHWALKNODETERMINATIONPROBABILITY, GRAPHWALKNODETERMINATIONPROBABILITYDEFAULT)
  val MAXPATHLENGTHTC:String = "maxpathlength"
  val UNTILCYCLETC:String = "untilcycle"
  val ALLTC:String = "all"

  val mapAppBudget: Double = getConfigEntry(SPSConstants.configCostRewards,MALAPPBUDGET, MALAPPBUDGETDEFAULT)
  val costOfDetection: Double = getConfigEntry(SPSConstants.configCostRewards,COSTOFDETECTION, COSTOFDETECTIONDEFAULT)
  val serviceReward: Double = getConfigEntry(SPSConstants.configCostRewards,SERVICEREWARD, SERVICEREWARDDEFAULT)
  val serviceRewardProbability: Double = getConfigEntry(SPSConstants.configCostRewards,SERVICEREWARDPROBABILITY, SERVICEREWARDPROBABILITYDEFAULT)
  val servicePenalty: Double = getConfigEntry(SPSConstants.configCostRewards,SERVICEPENALTY, SERVICEPENALTYDEFAULT)
  val targetAppScore: Double = getConfigEntry(SPSConstants.configCostRewards,TARGETAPPSCORE, TARGETAPPSCOREDEFAULT)
  val targetAppLowPenalty: Double = getConfigEntry(SPSConstants.configCostRewards,TARGETAPPLOWPENALTY, TARGETAPPLOWPENALTYDEFAULT)
  val targetAppHighPenalty: Double = getConfigEntry(SPSConstants.configCostRewards,TARGETAPPHIGHPENALTY, TARGETAPPHIGHPENALTYDEFAULT)

  def getFields: Map[String, Double] = this.getClass.getDeclaredFields.filter(field => field.getType == classOf[Double]).map(field => field.getName -> field.get(this).asInstanceOf[Double]).toMap[String, Double] ++  this.getClass.getDeclaredFields.filter(field => field.getType == classOf[Int]).map(field => field.getName -> field.get(this).toString.toDouble).toMap[String, Double]

  def apply(forceLinkOrphans: Boolean = true): GapGraph = new GapModel().generateModel(forceLinkOrphans)

  def createAction(from: GuiObject, to: GuiObject): Action =
    val fCount = from.childrenCount
    val tCount = to.childrenCount
    val cost: Double = SupplierOfRandomness.randProbs(1).head
    require(cost >= 0 && cost <= 1)

    Action(SupplierOfRandomness.onDemand(maxv = actionRange),
      if fCount > 0 then SupplierOfRandomness.onDemand(maxv = fCount) else 0,
      if tCount > 0 then SupplierOfRandomness.onDemand(maxv = tCount) else 0,
      if SupplierOfRandomness.onDemand() % 2 == 0 then None else Some(SupplierOfRandomness.onDemand(maxv = propValueRange)),
      cost
    )

  //  each node of the graph is a GuiObject that corresponds to a GUI screen, which is a tree of GuiObjects
  @main def runGapModelAlgebra(args: String*): Unit =
    logger.info("File /Users/drmark/Library/CloudStorage/OneDrive-UniversityofIllinoisChicago/Github/SeaPhish/GapModelGenerator/src/main/scala/GapGraph/GapModelAlgebra.scala created at time 5:42 PM")
