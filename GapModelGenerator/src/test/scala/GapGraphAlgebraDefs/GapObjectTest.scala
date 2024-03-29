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

class GapObjectTest extends AnyFlatSpec with Matchers with MockitoSugar {
  val logger: Logger = CreateLogger(this.getClass)
  behavior of "Gap graph generation"

  it should "test a mock" in {
    val mockRandomizer = mock[SupplierOfRandomness.type ]
    when(mockRandomizer.onDemand()).thenReturn(1)
    mockRandomizer.onDemand() shouldBe 1
  }
}