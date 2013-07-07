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

package fr.iscpif.kdtree.algorithm

import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.content._
import scala.util.Random
import fr.iscpif.kdtree.content.{ TestPoint, Label }

trait KdTreeComputation extends Sampler with Evaluator {

  type CONTENT <: Label with TestPoint

  def apply(tree: Tree[CONTENT], contentBuilder: Point => CONTENT)(implicit rng: Random, m: Manifest[CONTENT]): Tree[CONTENT] = {
    def refine(tree: Tree[CONTENT]): Tree[CONTENT] = {
      import mutable._

      val leavesToRefine = tree.leavesToRefine(tree)

      if (leavesToRefine.isEmpty) tree
      else refine(
        tree.evaluateAndInsert(
          tree.root.zonesAndPathsToTest(leavesToRefine),
          evaluator(contentBuilder)
        )
      )
    }

    refine(tree.clone)
  }

}
