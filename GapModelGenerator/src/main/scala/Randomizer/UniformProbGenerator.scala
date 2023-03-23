package Randomizer

import scala.math.pow
import scala.util.Random

trait Randomizer(seed: Option[Long]):
  protected val generator: Random = seed match
    case Some(s) => Random(s)
    case None => Random()


class UniformProbGenerator(val seed: Option[Long] = None) extends Randomizer(seed):
  type GeneratedValues = Double | Int
  private def generateUniformProbabilities: LazyList[Double] = generator.nextDouble() #:: generateUniformProbabilities
  private def generateInts: LazyList[Int] = generator.nextInt() #:: generateInts
  private def uniformOrInts(ints: Boolean): () => LazyList[GeneratedValues] = if ints then () => generateInts else () => generateUniformProbabilities

object UniformProbGenerator:
  def createGenerator(seed: Option[Long] = None): UniformProbGenerator =
    new UniformProbGenerator(seed)
  def apply(gen: UniformProbGenerator, offset:Int = 0, szOfValues: Int = 1, ints: Boolean = false): (UniformProbGenerator, Int, List[Double|Int]) =
    if offset > Int.MaxValue - szOfValues then
      val newgen = createGenerator(Option(gen.seed.getOrElse(0L)+1))
      (newgen, szOfValues, newgen.uniformOrInts(ints)().slice(0, 0 + szOfValues).toList)
    else (gen, offset + szOfValues, gen.uniformOrInts(ints)().slice(offset, offset + szOfValues).toList)


