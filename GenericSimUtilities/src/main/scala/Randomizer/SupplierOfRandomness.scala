package Randomizer

import Utilz.SPSConstants
import Utilz.SPSConstants.SEED
import com.typesafe.config.ConfigFactory

import scala.util.Try
object SupplierOfRandomness {


  private val seed: Option[Long] = Try(SPSConstants.globalConfig.getLong(SEED)) match {
    case scala.util.Success(value) =>
      Try(value) match {
        case scala.util.Success(value) => Some(value.asInstanceOf[Long])
        case scala.util.Failure(_) => None
      }
    case scala.util.Failure(_) => None
  }


  val randomGenInts: (UniformProbGenerator, Int, List[Int]) = UniformProbGenerator(UniformProbGenerator.createGenerator(seed), ints = true) match {
    case (gen, offset, lstOfInts) => (gen, offset, lstOfInts.asInstanceOf[List[Int]])
  }
  val randomGenDoubles: (UniformProbGenerator, Int, List[Double]) = UniformProbGenerator(UniformProbGenerator.createGenerator(seed)) match {
    case (gen, offset, lstOfDoubles) => (gen, offset, lstOfDoubles.asInstanceOf[List[Double]])
  }
}

