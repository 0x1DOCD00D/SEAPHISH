package GapGraphAlgebraDefs

import Randomizer.SupplierOfRandomness
import Utilz.ConfigReader.getConfigEntry
import Utilz.CreateLogger
import Utilz.SPSConstants.{DEFAULTEDGEPROBABILITY, EDGEPROBABILITY}
import org.mockito.Mockito.{mock, when}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar

class GapModelAlgebraTest extends AnyFlatSpec with Matchers with MockitoSugar {
  val logger = CreateLogger(this.getClass)
  behavior of "Gap graph generation"

  it should "test a mock" in {
    import Utilz.ConfigReader
    val crMock = mock[GapModel]
    when(crMock.generateModel()).thenReturn(GapGraph(null,null))
    val graph = crMock.generateModel()
    graph shouldBe GapGraph(null,null)
  }

  it should "create a small gap graph" in {
    val graph: GapGraph = GapModelAlgebra()

    logger.info(s"Generated ${graph.totalNodes} nodes in the graph")
    val am = graph.adjacencyMatrix
    logger.info("\n" + graph.toCsv(am))
    logger.info(graph.degrees.mkString(", "))
    val outConnect: List[Int] = am.flatMap(nodeRow => List(nodeRow.filter(_ < Float.PositiveInfinity).length)).toList
    logger.info(outConnect.mkString(", "))

    GapModelAlgebra.statesTotal shouldBe 5
    GapModelAlgebra.edgeProbability shouldBe 0.3
    graph.totalNodes should be <= 5+1
    graph.degrees.map(_._2) shouldBe outConnect
    graph.degrees.maxBy(_._2) should be >= graph.degrees.maxBy(_._2)
  }

}
