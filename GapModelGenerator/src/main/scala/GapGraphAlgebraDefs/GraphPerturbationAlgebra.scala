package GapGraphAlgebraDefs

import GapGraphAlgebraDefs.GapModelAlgebra.{edgeProbability, maxBranchingFactor, maxDepth, maxProperties, propValueRange, targetAppHighPenalty, targetAppLowPenalty}
import GapGraphAlgebraDefs.GraphPerturbationAlgebra.{ACTIONS, EdgeAdded, EdgeModified, EdgeRemoved, ModificationRecord, NodeAdded, NodeModified, OriginalGapComponent, Perturbation, logger}
import Randomizer.SupplierOfRandomness
import Utilz.CreateLogger

import scala.util.{Failure, Success, Try}
import com.google.common.graph.*
import org.slf4j.Logger

import java.util
import scala.annotation.tailrec
import scala.collection.immutable.TreeSeqMap.OrderBy
import scala.collection.immutable.Vector
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

//  six types of perturbations: node modified, node removed, edge removed, edge is modified, new node is added with edges, new edge is added to some existing node
class GraphPerturbationAlgebra(originalModel: GapGraph):
  type GraphPerturbationTuple = (GapGraph, ModificationRecord)
  import GraphPerturbationAlgebra.*
  private val newModel: GapGraph = originalModel.copy()
  private val containsNode: GuiObject => Boolean = (node: GuiObject) => newModel.sm.nodes().asScala.toList.contains(node)
  private val distances: Map[GuiObject, Double] = newModel.distances().toSeq.filter(_._2 < Double.PositiveInfinity).sortBy(_._2).toMap
  private val testDistances: Map[GuiObject, Double] = originalModel.distances().toSeq.filter(_._2 < Double.PositiveInfinity).sortBy(_._2).toMap
  if testDistances != distances then logger.error(s"Test distances do not match the distances ${testDistances.toSet diff distances.toSet}") else logger.info(s"Test distances match the distances")
  private val (minDistance, maxDistance) = (distances.minBy(_._2)._2, distances.maxBy(_._2)._2)
  private val (tminDistance, tmaxDistance) = (testDistances.minBy(_._2)._2, testDistances.maxBy(_._2)._2)
  if minDistance != tminDistance || maxDistance != tmaxDistance then logger.error(s"Test distances $tmaxDistance, $tminDistance do not match the distances $maxDistance, $minDistance") else logger.info(s"Test distances match the distances")
  private val range: Double = maxDistance - minDistance
  def perturbModel(dissimulate: Boolean = false): GraphPerturbationTuple =
    require(GapModelAlgebra.perturbationCoeff > 0 && GapModelAlgebra.perturbationCoeff <= 1, "The perturbation coefficient must be between 0 and 1")
    require(GapModelAlgebra.distanceCoeff >= 0 && GapModelAlgebra.distanceCoeff <= 1, "The distance percentile must be between 0 and 1")

    if range < GapModelAlgebra.distanceSpreadThreshold then
      logger.error(s"The range of distances, $range must be greater than the threshold ${GapModelAlgebra.distanceSpreadThreshold}")
      (newModel, Vector())
    else
      //    Suppose that the max distance is 5 and the distance coefficient is 0.2.
      //    Then the min distance to apply perturbation is 5*0.2 = 1
      val minDistance2ApplyPerturbation = maxDistance * GapModelAlgebra.distanceCoeff
      val tminDistance2ApplyPerturbation = tmaxDistance * GapModelAlgebra.distanceCoeff
      logger.info(s"Min distance to apply perturbation is $minDistance2ApplyPerturbation vs test min distance $tminDistance2ApplyPerturbation")
      val nodesToApplyPerturbation: Seq[GuiObject] = distances.filter(_._2 >= minDistance2ApplyPerturbation).keySet.toSeq
      logger.info(s"Out of the total ${distances.size} nodes, perturbations can be applied to ${nodesToApplyPerturbation.size} nodes that are beyond the distance threshold of $minDistance2ApplyPerturbation")
      if nodesToApplyPerturbation.isEmpty then
        logger.error(s"No nodes exist beyond the distance threshold of $minDistance2ApplyPerturbation")
        (newModel, Vector())
      else
        val probs = SupplierOfRandomness.randProbs(2 * nodesToApplyPerturbation.length).map(_ < GapModelAlgebra.perturbationCoeff)
        val yesOrNo: Iterator[Boolean] = probs.iterator
        logger.info(s"Out of the ${nodesToApplyPerturbation.size} nodes, perturbations will be applied to ${probs.count(_ == true)} nodes")
        (newModel,
          nodesToApplyPerturbation.toList.foldLeft(Vector[(OriginalGapComponent, Perturbation)]()) {
              (acc, node) => if yesOrNo.hasNext && yesOrNo.next() then perturbNode(node, dissimulate) ++ acc else acc
          })
    end if

  private def perturbNode(node: GuiObject, dissimulate: Boolean = false): ModificationRecord =
    val op2do: ACTIONS = ACTIONS.fromOrdinal(SupplierOfRandomness.onDemand(maxv = ACTIONS.values.map(_.ordinal).toList.max))
    if dissimulate then
      logger.info(s"Dissimulating node $node for the target app")
      modifyNode(node)
    else
      logger.info(s"Applying perturbation $op2do on node $node")
      op2do match
        case ACTIONS.ADDNODE => if dissimulate then modifyNode(node) else addNode(node)
        case ACTIONS.MODIFYNODE => modifyNode(node)
        case ACTIONS.REMOVENODE => if dissimulate then modifyNode(node) else removeNode(node)
        case op => operationOnEdges(node, op, dissimulate)
  end perturbNode

  /*
  * Iterate through all nodes, and for each node, iterate through all its predecessors and successors.
  * If the predecessor or successor is the node to be removed, then mark the edge removed.
  * */
  private def removeNode(node: GuiObject): ModificationRecord =
    import scala.jdk.OptionConverters.*
    val modificationRecord:ModificationRecord = Vector((OriginalGapComponent(node), NodeRemoved(node))) ++ newModel.sm.predecessors(node).asScala.toList.flatMap {pn =>
      newModel.sm.edgeValue(pn, node).toScala match
        case Some(edge) =>
          Vector((OriginalGapComponent(node), EdgeRemoved(edge)))
        case None => None
    }.toVector ++ newModel.sm.successors(node).asScala.toList.flatMap { sn =>
      newModel.sm.edgeValue(node, sn).toScala match
        case Some(edge) =>
          Vector((OriginalGapComponent(node), EdgeRemoved(edge)))
        case None => None
    }.toVector

    if newModel.sm.removeNode(node) then modificationRecord
    else
      logger.error(s"Failed to remove node $node")
      Vector()

  private def addNode(node: GuiObject): ModificationRecord =
    val newNode: GuiObject = GuiObject(newModel.sm.nodes().asScala.map(_.id).max + 1, SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
      SupplierOfRandomness.onDemand(maxv = maxProperties), propValueRange = SupplierOfRandomness.onDemand(maxv = propValueRange),
      maxDepth = SupplierOfRandomness.onDemand(maxv = maxDepth), maxBranchingFactor = SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
      maxProperties = SupplierOfRandomness.onDemand(maxv = maxProperties))
    val newEdge: Action = GapModelAlgebra.createAction(node, newNode)
    if newModel.sm.addNode(newNode) then
      Try(newModel.sm.putEdgeValue(node, newNode, newEdge)) match
        case Success(_) => Vector((OriginalGapComponent(node), NodeAdded(newNode)), (OriginalGapComponent(node), EdgeAdded(newEdge)))
        case Failure(exception) =>
          logger.error(s"Failed to add edge $newEdge for new node $newNode")
          Vector()
    else
      logger.error(s"Failed to add node $newNode")
      Vector()

  private def modifyNode(node: GuiObject): ModificationRecord =
    import scala.jdk.OptionConverters.*
    val modifiedNode: GuiObject = node.modify
    val adjacentNodes = newModel.sm.adjacentNodes(node).asScala.toList ::: List(node)
    logger.info(s"Adjacent nodes of $node are $adjacentNodes")
    logger.info(s"Modified version of the node $node is the node $modifiedNode")
    val inducedGraph: MutableValueGraph[GuiObject, Action] = Graphs.inducedSubgraph(newModel.sm, adjacentNodes.toSet.asJava)
    val preds = inducedGraph.predecessors(node).asScala.toList
    val succ = inducedGraph.successors(node).asScala.toList
    newModel.sm.removeNode(node)
    newModel.sm.addNode(modifiedNode)
    preds.foreach(pred => newModel.sm.putEdgeValue(pred, modifiedNode, inducedGraph.edgeValue(pred, node).get))
    succ.foreach(succ => newModel.sm.putEdgeValue(modifiedNode, succ, inducedGraph.edgeValue(node, succ).get))
    Vector((OriginalGapComponent(node), NodeModified(modifiedNode)))

  private def doTheEdge(node: GuiObject, foundNodes: Array[GuiObject], op: (GuiObject, GuiObject)=>ModificationRecord): ModificationRecord =
    if foundNodes.nonEmpty then
      if foundNodes.length == 1 then
        val chosenNode: GuiObject = foundNodes.head
        op(node, chosenNode)
      else
        val chosenNode: GuiObject = foundNodes(SupplierOfRandomness.onDemand(maxv = foundNodes.length))
        op(node, chosenNode)
    else Vector()

  private def operationOnEdges(node: GuiObject, action: ACTIONS, dissimulate: Boolean = false): ModificationRecord =
    import scala.jdk.OptionConverters.*

    val nodesLambda: GuiObject => Boolean = (otherNode: GuiObject) => otherNode != node &&
      (newModel.sm.hasEdgeConnecting(node, otherNode) ||
        newModel.sm.hasEdgeConnecting(otherNode, node))
    if containsNode(node) then
      val allNodes: List[GuiObject] = newModel.sm.nodes().asScala.toList
      if dissimulate then doTheEdge(node, allNodes.filterNot(nodesLambda).toArray[GuiObject], modifyEdge)
      else
        action match
          case ACTIONS.ADDEDGE => doTheEdge(node, allNodes.filter(nodesLambda).toArray[GuiObject], addEdge)
          case ACTIONS.REMOVEEDGE => doTheEdge(node, allNodes.filterNot(nodesLambda).toArray[GuiObject], removeEdge)
          case ACTIONS.MODIFYEDGE => doTheEdge(node, allNodes.filterNot(nodesLambda).toArray[GuiObject], modifyEdge)
          case _ =>
            logger.error(s"Invalid action $action")
            Vector()
    else
      logger.error(s"Node $node does not exist in the model")
      Vector()
  @tailrec
  private def removeEdge(node: GuiObject, chosenNode: GuiObject): ModificationRecord =
    import scala.jdk.OptionConverters.*
    if newModel.sm.hasEdgeConnecting(node, chosenNode) then
      val edge2Remove: Option[Action] = newModel.sm.edgeValue(node, chosenNode).toScala
      if edge2Remove.isDefined then
        if newModel.sm.removeEdge(node, chosenNode) == null then
          logger.error(s"Failed to remove edge from $node to $chosenNode because it does not exist")
          Vector()
        else
          logger.info(s"Removed an edge from $node to $chosenNode successfully to create a new perturbed model")
          Vector((OriginalGapComponent(node), EdgeRemoved(edge2Remove.get)))
      else
        logger.error(s"No edge exist from $node to $chosenNode but it should be there")
        Vector()
    else if newModel.sm.hasEdgeConnecting(chosenNode, node) then removeEdge(chosenNode, node)
    else Vector()

  private def addEdge(node: GuiObject, chosenNode: GuiObject): ModificationRecord =
    import scala.jdk.OptionConverters.*
    val edge2Modify: Option[Action] = newModel.sm.edgeValue(node, chosenNode).toScala
    val newEdge: Action = if edge2Modify.isEmpty then GapModelAlgebra.createAction(node, chosenNode) else edge2Modify.get.modify
    if newModel.sm.putEdgeValue(node, chosenNode, newEdge) == null then
      logger.info(s"Added edge $newEdge from $node to $chosenNode successfully to create a new perturbed model")
      Vector((OriginalGapComponent(node), EdgeAdded(newEdge)))
    else
      logger.info(s"Former edge from $node to $chosenNode was $edge2Modify and it's replaced by the new edge $newEdge")
      Vector((OriginalGapComponent(node), EdgeModified(edge2Modify.get))) ++ removeEdge(node, chosenNode) ++ addEdge(node, chosenNode)

  @tailrec
  private def modifyEdge(node: GuiObject, chosenNode: GuiObject): ModificationRecord =
    import scala.jdk.OptionConverters.*
    if newModel.sm.hasEdgeConnecting(node, chosenNode) then
      val edge2Modify: Option[Action] = newModel.sm.edgeValue(node, chosenNode).toScala
      if edge2Modify.isEmpty then
        logger.error(s"No edge exist from $node to $chosenNode but it should be there")
        Vector()
      else
        Vector((OriginalGapComponent(node), EdgeModified(edge2Modify.get))) ++ removeEdge(node, chosenNode) ++ addEdge(node, chosenNode)
    else modifyEdge(chosenNode, node)

object GraphPerturbationAlgebra:
  trait Perturbation

  case class NodeModified(node: GuiObject) extends Perturbation
  case class NodeRemoved(node: GuiObject) extends Perturbation
  case class NodeAdded(node: GuiObject) extends Perturbation
  case class EdgeRemoved(edge: Action) extends Perturbation
  case class EdgeAdded(edge: Action) extends Perturbation
  case class EdgeModified(action: Action) extends Perturbation

  case class OriginalGapComponent(node: GapGraphComponent)

  type ModificationRecord = Vector[(OriginalGapComponent, Perturbation)]
  type ModificationRecordInverse = Map[GapGraphComponent, Double]

  enum ACTIONS:
    case REMOVENODE, ADDNODE, MODIFYNODE, REMOVEEDGE, ADDEDGE, MODIFYEDGE

  val logger: Logger = CreateLogger(classOf[GraphPerturbationAlgebra.type])

  def apply(graph: GapGraph, dissimulate: Boolean = false): (GapGraph, ModificationRecord) =
    new GraphPerturbationAlgebra(graph).perturbModel(dissimulate)

  def inverseMR(mr: ModificationRecord): ModificationRecordInverse =
    def gapComponentFromPerturbation(perturbation: Perturbation): (GapGraphComponent, Double) = perturbation match
      case NodeModified(node) => (node, targetAppLowPenalty)
      case NodeRemoved(node) => (node, targetAppHighPenalty)
      case NodeAdded(node) => (node, targetAppLowPenalty)
      case EdgeRemoved(edge) => (edge, (targetAppHighPenalty+edge.cost)/2)
      case EdgeAdded(edge) => (edge, (targetAppLowPenalty+edge.cost)/2)
      case EdgeModified(edge) => (edge, (targetAppLowPenalty+edge.cost)/2)
    end gapComponentFromPerturbation

    logger.info(s"MR: ${mr.length} perturbations ")
    mr.foldLeft(Map[GapGraphComponent, Double]())(
      (acc, elem) => {
        val guicomp: (GapGraphComponent, Double) = gapComponentFromPerturbation(elem._2)
        acc + (guicomp._1 -> (acc.getOrElse(guicomp._1, 0.0d) + guicomp._2))
      }
    )