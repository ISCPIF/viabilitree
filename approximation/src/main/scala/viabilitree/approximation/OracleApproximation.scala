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

package viabilitree.approximation

import monocle.macros._
import viabilitree.kdtree._

case class OracleApproximation(
  depth: Int,
  box: Zone,
  oracle: Oracle,
  domain: Domain = InfiniteDomain,
  point: Option[Vector[Double]] = None,
  neutralBoundary: NeutralBoundary = NeutralBoundary.empty)

object OracleApproximationContent {
  implicit def containsLabel: ContainsLabel[OracleApproximationContent] = ContainsLabel[OracleApproximationContent](OracleApproximationContent.label.get)
}

@Lenses case class OracleApproximationContent(testPoint: Vector[Double], label: Boolean)

//trait OracleApproximation extends KdTreeComputation with RandomSampler with Input {
//
//  type CONTENT = OracleApproximation.Content
//
//
//  def buildContent(point: Point, label: Boolean): CONTENT = OracleApproximation.Content(point, label)
//
//  def label = Lenser[CONTENT](_.label)
//
//  def oracle(p: Point): Boolean
//
//  def seed = 42
//
//  implicit lazy val rng = new Random(seed)
//
//  def apply(implicit rng: Random, m: Manifest[CONTENT]): Option[Tree[CONTENT]] =
//    initialTree(p => contentBuilder(p)).map(t => apply(t, p => contentBuilder(p)))
//
//}
