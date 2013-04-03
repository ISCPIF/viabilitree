/*
 * Copyright (C) 03/04/13 Romain Reuillon
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

import fr.iscpif.kdtree.content._
import fr.iscpif.kdtree.structure._

import java.util.Random

object Circle extends App {

  case class Content(testPoint: Point, label: Boolean) extends Label with TestPoint

  def sampler(z: Zone, rng: Random) = z.randomPoint(rng)

  def oracle(p: Point) = Content(p, math.hypot(p(0), p(1)) <= 1)

  def evaluator(ps: Iterable[Zone], rng: Random) = ps.map(sampler(_, rng)).map(oracle)

  val originalNode =
    Leaf[Content](
      Content(Seq(0.0, 0.0), label = true),
      -2.0 -> 2.0, -2.0 -> 2.0
    )

  implicit val rng = new Random(42)

  val res = originalNode.compute(10, evaluator)

  println(res.volume)

}
