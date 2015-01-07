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

package fr.iscpif.viability

import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.content._
import monocle.SimpleLens
import math._
import scala.util.Random
import fr.iscpif.kdtree.algorithm.{KdTreeHandlingComputation, Sampler, KdTreeComputation}
import fr.iscpif.viability.control.{ ControlledDynamicContent, ControlTesting }

trait TreeRefinement <: Dynamic with Sampler with ControlTesting with ControlledDynamicContent { refine =>

  def kdTreeComputation: KdTreeHandlingComputation {  type CONTENT = refine.CONTENT  }

  def dimension: Int

  def lipschitz: Option[Double] = None

  def dilations: Int =
    lipschitz match {
      case Some(l) => (floor(l * sqrt(dimension) / 2) + 1).toInt
      case None => 0
    }

  def shouldBeReassigned(c: CONTENT): Boolean

  def viableFunction(tree: Tree[CONTENT]) = tree.label(_)

  def timeStep(tree: Tree[CONTENT])(implicit rng: Random): Option[Tree[CONTENT]] = {
    val viable = viableFunction(kdTreeComputation.dilate(tree, dilations))

    val reassignedTree =
      tree.reassign(
        content =>
          if (shouldBeReassigned(content)) {
            val label = findViableControl(content, viable, tree)
            label
          } else content
      )

    //TODO: May want to use (tree => findViableControl) function in order to benefit from optimised heuristic for viable control research.
    def contentBuilder(p: Point) = exhaustiveFindViableControl(p, viable)

    kdTreeComputation.findTrueLabel(reassignedTree, contentBuilder).map {
      tree => kdTreeComputation.learnBoundary(tree, contentBuilder)
    }
  }

  def erodeInDomain(t: Tree[CONTENT])(implicit rng: Random): Tree[CONTENT] = kdTreeComputation.erodeInDomain(t)
  def erodeInDomain(t: Tree[CONTENT], n: Int)(implicit rng: Random): Tree[CONTENT] = kdTreeComputation.erodeInDomain(t,n)

  def apply(tree: Tree[CONTENT])(implicit rng: Random, m: Manifest[CONTENT]): Option[Tree[CONTENT]] = timeStep(tree)
}

