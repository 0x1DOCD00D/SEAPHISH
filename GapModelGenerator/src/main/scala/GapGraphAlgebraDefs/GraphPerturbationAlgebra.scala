package GapGraphAlgebraDefs

import GapGraphAlgebraDefs.GapModelAlgebra.{edgeProbability, maxBranchingFactor, maxDepth, maxProperties, propValueRange}
import GapGraphAlgebraDefs.GraphPerturbationAlgebra.{ACTIONS, EdgeAdded, EdgeModified, EdgeRemoved, ModificationRecord, NodeAdded, NodeModified, OriginalGapComponent, Perturbation, logger}
import Randomizer.SupplierOfRandomness
import Utilz.CreateLogger

import scala.util.{Failure, Success, Try}
import com.google.common.graph.*
import org.slf4j.Logger

import scala.annotation.tailrec
import scala.collection.immutable.TreeSeqMap.OrderBy
import scala.collection.immutable.Vector
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

//  six types of perturbations: node modified, node removed, edge removed, edge is modified, new node is added with edges, new edge is added to some existing node
class GraphPerturbationAlgebra(originalModel: GapGraph):
  import GraphPerturbationAlgebra.*
  private val newModel: GapGraph = originalModel.copy()
  private val distances: Map[GuiObject, Double] = newModel.distances().toSeq.sortBy(_._2).toMap
  private val (minDistance, maxDistance) = (distances.minBy(_._2)._2, distances.maxBy(_._2)._2)
  private val range: Double = maxDistance - minDistance
  def perturbModel: (GapGraph, ModificationRecord) =
    require(GapModelAlgebra.perturbationCoeff > 0 && GapModelAlgebra.perturbationCoeff <= 1, "The perturbation coefficient must be between 0 and 1")
    require(GapModelAlgebra.distanceCoeff >= 0 && GapModelAlgebra.distanceCoeff <= 1, "The distance percentile must be between 0 and 1")

    if range < GapModelAlgebra.distanceSpreadThreshold then
      logger.error(s"The range of distances, $range must be greater than the threshold ${GapModelAlgebra.distanceSpreadThreshold}")
      (newModel, Vector())
    else
      //    Suppose that the max distance is 5 and the distance coefficient is 0.2.
      //    Then the min distance to apply perturbation is 5*0.2 = 1
      val minDistance2ApplyPerturbation = maxDistance * GapModelAlgebra.distanceCoeff
      val nodesToApplyPerturbation: Seq[GuiObject] = distances.filter(_._2 >= minDistance2ApplyPerturbation).keySet.toSeq
      if nodesToApplyPerturbation.isEmpty then
        logger.error(s"No nodes exist beyond the distance threshold of $minDistance2ApplyPerturbation")
        (newModel, Vector())
      else
        val yesOrNo: Iterator[Boolean] = SupplierOfRandomness.randProbs(2 * nodesToApplyPerturbation.length).map(_ >= GapModelAlgebra.perturbationCoeff).iterator
        (newModel, nodesToApplyPerturbation.toList.foldLeft(Vector[(OriginalGapComponent, Perturbation)]())((acc, node) => if yesOrNo.next() then perturbNode(node) ++ acc else acc))
    end if

  private def perturbNode(node: GuiObject): ModificationRecord =
    ACTIONS.fromOrdinal(SupplierOfRandomness.onDemand(ACTIONS.values.map(_.ordinal).toList.max)) match
      case ACTIONS.ADDNODE => addNode(node)
      case ACTIONS.MODIFYNODE => modifyNode(node)
      case ACTIONS.REMOVENODE => removeNode(node)
      case op => operationOnEdges(node, op)

  /*
  * Iterate through all nodes, and for each node, iterate through all its predecessors and successors.
  * If the predecessor or successor is the node to be removed, then mark the edge removed.
  * */
  private def removeNode(node: GuiObject): ModificationRecord =
    import scala.jdk.OptionConverters.*
    val allNodes: List[GuiObject] = newModel.sm.nodes().asScala.toList
    if allNodes.contains(node) then
      val modificationRecord: ModificationRecord = allNodes.map {
        otherNode =>
          if otherNode != node && newModel.sm.hasEdgeConnecting(node, otherNode) then
            newModel.sm.edgeValue(node, otherNode).asInstanceOf[Option[Action]] match
              case Some(edge) =>
                (OriginalGapComponent(edge), EdgeRemoved(edge))
              case None => None
          else if otherNode != node && newModel.sm.hasEdgeConnecting(otherNode, node) then
            newModel.sm.edgeValue(otherNode, node).toScala match
              case Some(edge) =>
                (OriginalGapComponent(edge), EdgeRemoved(edge))
              case None => None
          else None
      }.toVector.asInstanceOf[ModificationRecord]
      if newModel.sm.removeNode(node) then modificationRecord
      else
        logger.error(s"Failed to remove node $node")
        Vector()
    else
      logger.error(s"Node $node does not exist in the model")
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
    val adjacentNodes = newModel.sm.adjacentNodes(node)
    Try(adjacentNodes.add(node)) match
      case Success(_) =>
        val inducedGraph: MutableValueGraph[GuiObject, Action] = Graphs.inducedSubgraph(newModel.sm, adjacentNodes)
        val preds = inducedGraph.predecessors(node).asScala.toList
        val succ = inducedGraph.successors(node).asScala.toList
        newModel.sm.removeNode(node)
        newModel.sm.addNode(modifiedNode)
        preds.foreach(pred => newModel.sm.putEdgeValue(pred, modifiedNode, inducedGraph.edgeValue(pred, node).get))
        succ.foreach(succ => newModel.sm.putEdgeValue(modifiedNode, succ, inducedGraph.edgeValue(node, succ).get))
        Vector((OriginalGapComponent(node), NodeModified(modifiedNode)))
      case Failure(exception) =>
        logger.error(s"Failed to modify node $node when obtaining the adjacent nodes for reason $exception")
        Vector()


  private def operationOnEdges(node: GuiObject, action: ACTIONS): ModificationRecord =
    import scala.jdk.OptionConverters.*
    val allNodes: List[GuiObject] = newModel.sm.nodes().asScala.toList
    val nodesLambda: GuiObject => Boolean = (otherNode: GuiObject) => otherNode != node &&
      (newModel.sm.hasEdgeConnecting(node, otherNode) ||
        newModel.sm.hasEdgeConnecting(otherNode, node))
    if allNodes.contains(node) then
      action match
        case ACTIONS.ADDEDGE =>
          val foundNodes: Array[GuiObject] = allNodes.filterNot(nodesLambda).toArray[GuiObject]
          if foundNodes.nonEmpty then
            val chosenNode: GuiObject = foundNodes(SupplierOfRandomness.onDemand(maxv = foundNodes.length))
            addEdge(node, chosenNode)
          else Vector()
        case ACTIONS.REMOVEEDGE =>
          val foundNodes: Array[GuiObject] = allNodes.filter(nodesLambda).toArray[GuiObject]
          if foundNodes.nonEmpty then
            val chosenNode: GuiObject = foundNodes(SupplierOfRandomness.onDemand(maxv = foundNodes.length))
            removeEdge(node, chosenNode)
          else Vector()
        case ACTIONS.MODIFYNODE =>
          val foundNodes: Array[GuiObject] = allNodes.filter(nodesLambda).toArray[GuiObject]
          if foundNodes.nonEmpty then
            val chosenNode: GuiObject = foundNodes(SupplierOfRandomness.onDemand(maxv = foundNodes.length))
            modifyEdge(node, chosenNode)
          else Vector()
        case _ => Vector()
    else Vector()
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
    else removeEdge(chosenNode, node)

  private def addEdge(node: GuiObject, chosenNode: GuiObject, newEdge2Add: Option[Action] = None): ModificationRecord =
    if newModel.sm.hasEdgeConnecting(node, chosenNode) then
      logger.error(s"Edge from $node to $chosenNode already exists and new edge cannot be added")
      Vector()
    else
      val newEdge: Action = if newEdge2Add.isEmpty then GapModelAlgebra.createAction(node, chosenNode) else newEdge2Add.get
      if newModel.sm.putEdgeValue(node, chosenNode, newEdge) == null then
        logger.info(s"Added edge $newEdge from $node to $chosenNode successfully to create a new perturbed model")
        Vector((OriginalGapComponent(node), EdgeAdded(newEdge)))
      else
        logger.error(s"Failed to add edge from $node to $chosenNode")
        Vector()

  @tailrec
  private def modifyEdge(node: GuiObject, chosenNode: GuiObject): ModificationRecord =
    import scala.jdk.OptionConverters.*
    if newModel.sm.hasEdgeConnecting(node, chosenNode) then
      val edge2Modify: Option[Action] = newModel.sm.edgeValue(node, chosenNode).toScala
      if edge2Modify.isEmpty then
        logger.error(s"No edge exist from $node to $chosenNode but it should be there")
        Vector()
      else
        val modifiedEdge: Action = edge2Modify.get.modify
        Vector((OriginalGapComponent(node), EdgeModified(modifiedEdge))) ++ removeEdge(node, chosenNode) ++ addEdge(node, chosenNode, Some(modifiedEdge))
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


  enum ACTIONS:
    case REMOVENODE, ADDNODE, MODIFYNODE, REMOVEEDGE, ADDEDGE, MODIFYEDGE

  val logger: Logger = CreateLogger(classOf[GraphPerturbationAlgebra.type])

  def apply(originalModel: GapGraph): (GapGraph, ModificationRecord) =
    new GraphPerturbationAlgebra(originalModel).perturbModel