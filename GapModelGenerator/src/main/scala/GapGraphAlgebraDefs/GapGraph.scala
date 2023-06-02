package GapGraphAlgebraDefs

import Randomizer.SupplierOfRandomness
import Utilz.CreateLogger
import com.google.common.graph.{Graphs, MutableValueGraph, ValueGraphBuilder}
import org.slf4j.Logger

import scala.collection.immutable.TreeSeqMap.OrderBy
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

case class GapGraph(sm: GuiStateMachine, initState: GuiObject):
  val logger: Logger = CreateLogger(classOf[GapGraph])
  def copy: GapGraph = GapGraph(Graphs.copyOf(sm), initState)
  def degrees: List[(Int, Int)] = sm.nodes().asScala.toList.map(node => (sm.inDegree(node), sm.outDegree(node)))

  def totalNodes: Int = sm.nodes().asScala.count(_ => true)

  def adjacencyMatrix: Array[Array[Float]] =
    val nodes: Array[GuiObject] = sm.nodes().asScala.toArray
    val matrix: Array[Array[Float]] = Array.ofDim[Float](nodes.length, nodes.length)
    nodes.indices.foreach(i =>
      nodes.indices.foreach(j =>
        if sm.hasEdgeConnecting(nodes(i), nodes(j)) then matrix(i)(j) = sm.edgeValue(nodes(i), nodes(j)).get.cost.toFloat
        else matrix(i)(j) = Float.PositiveInfinity
      )
    )
    matrix

  extension (m: Array[Array[Float]])
    def toCsv: String =
      val sb = new StringBuilder
      m.foreach(row =>
        sb.append("\n")
        row.foreach(cell => if cell != Float.PositiveInfinity then sb.append(f"$cell%.3f").append(",") else sb.append("-").append(",")))
      sb.toString

  def maxOutDegree(): Int = sm.nodes().asScala.map(node => sm.outDegree(node)).max

  def getRandomConnectedNode(from: GuiObject): Option[(GuiObject, Action)] =
    val successors: Array[GuiObject] = sm.successors(from).asScala.toArray
    if successors.isEmpty then None
    else
      val randomSuccessor: GuiObject = successors(SupplierOfRandomness.onDemand(maxv = successors.length))
      val edge: Action = sm.edgeValue(from, randomSuccessor).get
      Some((randomSuccessor, edge))

  def unreachableNodes(): (Set[GuiObject], Int) =
    def dfs(nodes: List[GuiObject], visited: Set[GuiObject]): (Set[GuiObject], Int) =
      nodes match
        case Nil => (visited, 0)
        case hd :: tl => if visited.contains(hd) then (visited, 1) else
          val (nodesHd, loopsHd) = dfs(sm.successors(hd).asScala.toList, visited + hd)
          val (nodesTl, loopsTl) = dfs(tl, visited + hd)
          (nodesHd ++ nodesTl, loopsHd + loopsTl)
    end dfs

    val (reachableNodes: Set[GuiObject], loops: Int) = dfs(sm.successors(initState).asScala.toList, Set())
    val allNodes: Set[GuiObject] = sm.nodes().asScala.toSet
    (allNodes -- reachableNodes -- Set(initState), loops)

  def distances(): Map[GuiObject, Double] =
    val distanceMap: scala.collection.mutable.Map[GuiObject, Double] = collection.mutable.Map() ++ sm.nodes().asScala.map(node => node -> Double.PositiveInfinity).toMap
    val zeroCost: Double = 0.0d
    val noEdgeCost: Double = Double.NaN
    distanceMap += (initState -> zeroCost)

    def relax(u: GuiObject)(v: GuiObject): Boolean =
      import scala.jdk.OptionConverters.*
      val edgeCost = if sm.hasEdgeConnecting(u, v) then
        sm.edgeValue(u, v).toScala match
          case Some(action) => action.cost
          case None => noEdgeCost
      else noEdgeCost
      if edgeCost.isNaN then false
      else if distanceMap(v) > distanceMap(u) + edgeCost then
        distanceMap(v) = distanceMap(u) + edgeCost
        true
      else false
    end relax

    def explore(node: GuiObject): Unit =
      require(node != null, "The GuiObject node must not be null")
      val successors = sm.successors(node).asScala.toList
      val relaxNode: GuiObject => Boolean = relax(node)
      successors match
        case Nil => ()
        case hd :: tl => if relaxNode(hd) then explore(hd)
          tl.foreach(cn => if relaxNode(cn) then explore(cn) else ())
    end explore

    explore(initState)
    distanceMap.toMap
