package viabilitree.approximation

import viabilitree.kdtree._

object evaluator {

  def sequential[CONTENT](contentBuilder: Vector[Double] => CONTENT, sampler: Sampler): Evaluator[CONTENT] =
    (zone: Vector[Zone], rng: util.Random) => zone.map { z => contentBuilder(sampler(z, rng)) }

  //  def parallel[CONTENT](contentBuilder: Vector[Double] => CONTENT, sampler: Sampler): Evaluator[CONTENT] = (zone: Vector[Zone], rng: Random) => {
  //    def random(seed: Long) = new Random(new RandomAdaptor(new Well44497b(seed)))
  //    val seeds = Iterable.fill(zone.size)(rng.nextLong)
  //
  //    (zone zip seeds).par.map {
  //    case (z, seed) =>
  //      val point = sampler(z, random(seed))
  //      contentBuilder(point)
  //    }.seq
  //  }

}
