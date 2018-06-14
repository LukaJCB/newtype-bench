package bench

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import newtype.V
import cats.implicits._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class ParallelEitherBench {

  def longList: List[Int] =
    (1 to 10000).toList

  def nToEither(n: Int) = if (n % 3 == 0) Right(n) else Left("H")

  @Benchmark
  def currentBench = longList.parTraverse(nToEither)

  @Benchmark
  def newtypeBench = V.toEither(longList.traverse(n => V.fromEither(nToEither(n))))

}
