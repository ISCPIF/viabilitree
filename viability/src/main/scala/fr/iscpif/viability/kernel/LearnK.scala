/*
 * Copyright (C) 2014 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


package fr.iscpif.viability.kernel

import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import fr.iscpif.viability.K
import fr.iscpif.viability.control.ControlledDynamicContent

import scala.util.Random

trait LearnK <: ViabilityKernel with K with Input {

  override def tree0(implicit rng: Random) = {
    def contentBuilder(p: Point) = ControlledDynamicContent.Content(p, None, None, k(p), 0)
    initialTree(contentBuilder).map(kdTreeComputation.learnBoundary(_, contentBuilder))
  }

}
