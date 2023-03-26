package GapGraphAlgebra

import scala.util.{Failure, Success, Try}
import com.google.common.graph.*

import scala.collection.immutable.TreeSeqMap.OrderBy
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object GraphPerturbationAlgebra:
  trait Perturbation

  case class NodeRemoved(node: GuiObject) extends Perturbation

  case class NodeAdded(node: GuiObject) extends Perturbation

  case class EdgeRemoved(edge: Action) extends Perturbation

  case class EdgeAdded(edge: Action) extends Perturbation

  case class OriginalGapComponent(node: Option[GapGraphComponent])

  type ModificationRecord = Map[OriginalGapComponent, Perturbation]

  //  six types of perturbations: node modified, node removed, edge removed, edge is modified, new node is added with edges, new edge is added to some existing node
/*
  def perturbModel(model: GapGraph, perturbationCoefficient: Double, distancePercentile: Double): (GapGraph, ModificationRecord) =
    require(perturbationCoefficient > 0 && perturbationCoefficient <= 1, "The perturbation coefficient must be between 0 and 1")
    require(distancePercentile > 0 && distancePercentile <= 1, "The distance percentile must be between 0 and 1")
    val distances: Map[GuiObject, Double] = model.distances().toSeq.sortBy(_._2).toMap
    val (minDistance, maxDistance) = (distances.minBy(_._2)._2, distances.maxBy(_._2)._2)
    val modelClone = model.copy()

    def verifyDistance(node: GuiObject): Boolean =
      val range: Double = maxDistance - minDistance
      require(range > 1E-20, "The range of distances must be greater than 0")
      val percentile = (distances(node) - minDistance) / range
      percentile >= distancePercentile

    def chooseNodeRandomlyAtDistance(): GuiObject =
      val nodesAtDistance: Array[GuiObject] = distances.filter(_._2 >= distancePercentile).keys.toArray
      nodesAtDistance(scala.util.Random.nextInt(nodesAtDistance.length))

    def removeNode(node: GuiObject, modification: ModificationRecord): ModificationRecord = if modelClone.sm.removeNode(node) then
      modification + (OriginalGapComponent(Some(node)) -> NodeRemoved(node)) else modification

    def addNode(modification: ModificationRecord): ModificationRecord =
      val newNode: GuiObject = GuiObject(modelClone.sm.nodes().asScala.map(_.id).max + 1, scala.util.Random.nextInt(maxBranchingFactor), scala.util.Random.nextInt(maxProperties))
      val allNodes: Array[GuiObject] = modelClone.sm.nodes().asScala.toArray
      val modz = allNodes.foldLeft(Map[OriginalGapComponent, Modification]())((acc, node) =>
        if UniformProbGenerator().head < perturbationCoefficient && verifyDistance(node) then
          val edge = createAction(node, newNode)
          stateMachine.putEdgeValue(node, newNode, edge)
          acc + (OriginalGapComponent(None) -> EdgeAdded(edge))
        else acc
      )
      modification ++ modz + (OriginalGapComponent(None) -> NodeAdded(newNode))

    def modifyNode(modification: ModificationRecord): ModificationRecord =
      import scala.jdk.OptionConverters.*
      val allNodes: Array[GuiObject] = modelClone.sm.nodes().asScala.toArray
      val victim: GuiObject = allNodes(scala.util.Random.nextInt(allNodes.length))
      val newNode: GuiObject = GuiObject(victim.id, scala.util.Random.nextInt(maxBranchingFactor), scala.util.Random.nextInt(maxProperties))
      val pred = modelClone.sm.predecessors(victim).asScala
      val succ = modelClone.sm.successors(victim).asScala
      modelClone.sm.addNode(newNode)
      pred.foreach(node =>
        modelClone.sm.edgeValue(node, victim).toScala match
          case Some(edge) =>
            modelClone.sm.removeEdge(node, victim)
            modelClone.sm.putEdgeValue(node, newNode, edge)
            ()
          case None => ()
      )
      succ.foreach(node =>
        modelClone.sm.edgeValue(victim, node).toScala match
          case Some(edge) =>
            modelClone.sm.removeEdge(victim, node)
            modelClone.sm.putEdgeValue(newNode, node, edge)
            ()
          case None => ()
      )
      modification + (OriginalGapComponent(Some(victim)) -> NodeRemoved(victim)) + (OriginalGapComponent(Some(victim)) -> NodeAdded(newNode))


    def removeEdge(node: GuiObject, other: GuiObject, modification: ModificationRecord): ModificationRecord =
      val edge = modelClone.sm.removeEdge(node, other)
      if edge == null then modification else
        modification + (OriginalGapComponent(Some(edge)) -> EdgeRemoved(edge))

    def modifyEdge(node: GuiObject, other: GuiObject, modification: ModificationRecord): ModificationRecord =
      val edge = modelClone.sm.removeEdge(node, other)
      if edge == null then modification else
        val newEdge: Action = edge.copy(resultingValue = Some(scala.util.Random.nextInt(propValueRange)), cost = UniformProbGenerator().head)
        Try(modelClone.sm.putEdgeValue(node, other, newEdge)) match
          case Success(_) => modification + (OriginalGapComponent(Some(edge)) -> EdgeAdded(newEdge)) + (OriginalGapComponent(Some(edge)) -> EdgeRemoved(edge))
          case Failure(_) => modification + (OriginalGapComponent(Some(edge)) -> EdgeRemoved(edge))

    null
  //    (modelClone, removedEntities.toSet)

  end perturbModel

*/