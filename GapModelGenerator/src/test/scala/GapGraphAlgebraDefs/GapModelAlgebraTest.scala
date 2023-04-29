package GapGraphAlgebraDefs

import Randomizer.SupplierOfRandomness
import Utilz.ConfigReader.getConfigEntry
import Utilz.CreateLogger
import Utilz.SPSConstants.{DEFAULTEDGEPROBABILITY, EDGEPROBABILITY}
import org.mockito.Mockito.{mock, when}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.slf4j.Logger

class GapModelAlgebraTest extends AnyFlatSpec with Matchers with MockitoSugar {
  val logger: Logger = CreateLogger(this.getClass)
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

    val am = graph.adjacencyMatrix
    logger.info("\n" + graph.toCsv(am))
    graph.degrees.length shouldBe 6
    am.flatMap(nodeRow => List(nodeRow.count(_ < Float.PositiveInfinity))).toList.length shouldBe 6

    GapModelAlgebra.statesTotal shouldBe 5
    GapModelAlgebra.connectedness shouldBe 3
    GapModelAlgebra.edgeProbability shouldBe 0.3
    graph.totalNodes should be <= GapModelAlgebra.statesTotal+1
  }

}
