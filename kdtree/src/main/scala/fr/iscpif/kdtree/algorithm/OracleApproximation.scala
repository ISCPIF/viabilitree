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

package fr.iscpif.kdtree.algorithm

import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.content._
import scala.util.Random
import scalaz.Lens

trait OracleApproximation extends KdTreeComputation with RandomSampler with ParallelEvaluator with Input {

  case class Content(testPoint: Point, label: Boolean) extends Label with TestPoint

  type CONTENT = Content

  def contentBuilder(p: Point) = Content(p, oracle(p))
  def buildContent(point: Point, label: Boolean): CONTENT = Content(point, label)

  def label: Lens[CONTENT, Boolean] =
    Lens.lensu((c, v) => c.copy(label = v), _.label)

  def oracle(p: Point): Boolean

  def seed = 42

  implicit lazy val rng = new Random(seed)

  def apply(implicit rng: Random, m: Manifest[CONTENT]): Option[Tree[CONTENT]] =
    initialTree(p => contentBuilder(p)).map(t => apply(t, p => contentBuilder(p)))

}
