package GapGraphAlgebraDefs

import GapGraphAlgebraDefs.GraphPerturbationAlgebra.{ModificationRecord, NodeAdded, OriginalGapComponent}
import Randomizer.SupplierOfRandomness
import Utilz.ConfigReader.getConfigEntry
import Utilz.CreateLogger
import Utilz.SPSConstants.{DEFAULTEDGEPROBABILITY, EDGEPROBABILITY}
import com.google.common.graph.{MutableValueGraph, ValueGraphBuilder}
import org.mockito.Mockito.{mock, when}
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.slf4j.Logger

class GraphPerturbationAlgebraTest extends AnyFlatSpec with Matchers with MockitoSugar with PrivateMethodTester {
  val logger: Logger = CreateLogger(this.getClass)

  val node1: GuiObject = GuiObject(id = 1, children = 5, props = 10, propValueRange = 20, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10)
  val node2: GuiObject = GuiObject(id = 2, children = 5, props = 10, propValueRange = 20, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10)
  val node3: GuiObject = GuiObject(id = 3, children = 5, props = 10, propValueRange = 20, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10)
  val edge12: Action = Action(actionType = 1, fromId = 1, toId = 2, resultingValue = Some(12), cost = 0.12)
  val edge23: Action = Action(actionType = 2, fromId = 2, toId = 3, resultingValue = Some(23), cost = 0.23)
  def createTestGraph(): GapGraph = {
    val graph1: MutableValueGraph[GuiObject, Action] = ValueGraphBuilder.directed().build()

    if !graph1.addNode(node1) then
      logger.error(s"Node $node1 already exists")
    if !graph1.addNode(node2) then
      logger.error(s"Node $node2 already exists")
    if !graph1.addNode(node3) then
      logger.error(s"Node $node3 already exists")
    graph1.putEdgeValue(node1, node2, edge12)
    graph1.putEdgeValue(node2, node3, edge23)
    GapGraph(graph1, node1)
  }

  behavior of "Gap graph perturbation operations"

  it should "add a new node to the graph" in {
    val graph = createTestGraph()
    val algebra = new GraphPerturbationAlgebra(graph)
    val theAddNodeFunc = PrivateMethod[ModificationRecord](Symbol("addNode"))
    val modificationRecord:ModificationRecord = algebra invokePrivate theAddNodeFunc(node3)
    logger.info(modificationRecord.toString)
    logger.info(graph.sm.toString)
    modificationRecord.size shouldBe 2
    modificationRecord.find(_._1 == OriginalGapComponent(node3)) shouldBe Some(OriginalGapComponent(node3), NodeAdded(GuiObject(4,0,2,1,9,1,2,0,List(),List())))
  }
}
