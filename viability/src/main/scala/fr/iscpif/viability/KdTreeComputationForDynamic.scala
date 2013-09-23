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
import math._
import scala.util.Random
import fr.iscpif.kdtree.algorithm.KdTreeComputation

trait KdTreeComputationForDynamic extends KdTreeComputation with Dynamic with ControlTesting with ControlledDynamicContent {

  def dimension: Int

  def lipschitz: Option[Double] = None

  def dilatedTree(tree: Tree[CONTENT])(implicit m: Manifest[CONTENT]) = {
    def dilate(t: Tree[CONTENT], nb: Int): Tree[CONTENT] =
      if (nb <= 0) t
      else dilate(t.dilate, nb - 1)
    dilate(tree, dilations)
  }

  def dilations: Int =
    lipschitz match {
      case Some(l) => (floor(l * sqrt(dimension) / 2) + 1).toInt
      case None => 0
    }

  def shouldBeReassigned(c: CONTENT): Boolean

  def timeStep(tree: Tree[CONTENT])(implicit rng: Random, m: Manifest[CONTENT]): Option[Tree[CONTENT]] = {
    //TODO delete Romain
    println("time step : start ++++++++++++++++++++++++++++++++++++++")
    val dilated = dilatedTree(tree)

    def viable(p: Point): Boolean = dilated.label(p)

    val reassignedTree =
      tree.reassign(
        content =>
          if (shouldBeReassigned(content)) findViableControl(content, viable, tree)
          else content
      )

    println("end reassign begin findTrueLabel +++")

    def contentBuilder(p: Point) = exhaustiveFindViableControl(p, viable)

    findTrueLabel(reassignedTree, contentBuilder).map { tree => learnBoundary(tree, contentBuilder) }
  }

  def apply(tree: Tree[CONTENT])(implicit rng: Random, m: Manifest[CONTENT]): Option[Tree[CONTENT]] = timeStep(tree)
}
