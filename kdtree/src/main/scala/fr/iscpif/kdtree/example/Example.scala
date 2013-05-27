/*
 * Copyright (C) 27/05/13 Romain Reuillon
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

package fr.iscpif.kdtree.example

import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.content._
import scala.util.Random
import org.apache.commons.math3.random._

trait Example {

  sealed case class Content(testPoint: Point, label: Boolean) extends Label with TestPoint {
    def relabel(l: Boolean) = copy(label = l)
  }

  def sampler(z: Zone, rng: Random) = z.randomPoint(rng)

  def evaluator(ps: Iterable[Zone], rng: Random) = {
    val seeds = Iterable.fill(ps.size)(rng.nextLong)

    (ps zip seeds).par.map {
      case (z, seed) =>
        val point = sampler(z, random(seed))
        Content(point, oracle(point))
    }.seq
  }

  def oracle(p: Point): Boolean

  def random(seed: Long) = new Random(new RandomAdaptor(new Well44497b(seed)))

  def zone: Seq[Interval]
  def point: Point

  def depth: Int

  def originalNode =
    Leaf(
      Content(point, label = true),
      zone: _*
    )

  def res(implicit rng: Random): Node[Content] = originalNode.compute(depth, evaluator)

}
