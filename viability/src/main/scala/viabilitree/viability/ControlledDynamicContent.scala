/*
 * Copyright (C) 05/07/13 Romain Reuillon
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

package viabilitree.viability

import monocle.macros.Lenses
import viabilitree.kdtree.structure._

object ControlledDynamicContent {
  def reduce: ContentReduction[ControlledDynamicContent] =
    (c1: Leaf[ControlledDynamicContent], c2: Leaf[ControlledDynamicContent]) => Some(c1.content)


}

@Lenses case class ControlledDynamicContent(
  testPoint: Vector[Double],
  control: Option[Int],
  resultPoint: Option[Vector[Double]],
  label: Boolean,
  controlMax: Int)

