/*
 * Copyright (C) 24/06/13 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.kdtree.content

import fr.iscpif.kdtree.structure._
import scala.util.Random
import org.apache.commons.math3.random.{ Well44497b, RandomAdaptor }

trait KdTreeComputation {

  type T <: Label with TestPoint

  def sampler(z: Zone, rng: Random) = z.randomPoint(rng)

  def evaluator(ps: Iterable[Zone], rng: Random) = {
    val seeds = Iterable.fill(ps.size)(rng.nextLong)

    (ps zip seeds).par.map {
      case (z, seed) =>
        val point = sampler(z, random(seed))
        contentBuilder(point)
    }.seq
  }

  def contentBuilder(p: Point): T
  def originalTree: Tree[T]
  def random(seed: Long) = new Random(new RandomAdaptor(new Well44497b(seed)))

  def run(implicit rng: Random): Tree[T] =
    originalTree.compute(evaluator)

}
