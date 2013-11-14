/*
 * Copyright (C) 03/10/13 Romain Reuillon
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

package fr.iscpif.viability.kernel

import fr.iscpif.kdtree.structure._
import scala.util.Random

trait ConstraintSet { this: ViabilityKernel =>
  def constraints(p: Point): Boolean
  override def k(p: Point) = constraints(p)
  override def learnConstraintSet(tree: Tree[CONTENT])(implicit rng: Random) = {
    def contentBuilder(p: Point) = Content(p, None, None, k(p), 0)
    learnBoundary(tree, contentBuilder)
  }

}
