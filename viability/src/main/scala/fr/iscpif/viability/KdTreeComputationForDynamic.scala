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

trait KdTreeComputationForDynamic extends KdTreeComputation {

  type CONTENT <: TestPoint with ResultPoint with Label

  implicit val relabel: Relabeliser[CONTENT]

  def dynamic(p: Point): Point
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

  def buildContent(from: Point, result: Point, viable: Boolean): CONTENT

  def shouldBeReassigned(c: CONTENT): Boolean

  def apply(tree: Tree[CONTENT])(implicit rng: Random, m: Manifest[CONTENT]): Option[Tree[CONTENT]] = {
    val dilated = dilatedTree(tree)

    def viable(p: Point): Boolean = dilated.label(p)

    def contentBuilder(p: Point): CONTENT = {
      val result = dynamic(p)
      buildContent(p, result, viable(result))
    }

    val reassignedTree =
      tree.reassign(
        content =>
          if (shouldBeReassigned(content)) relabel(content, t => viable(t.result))
          else content
      )

    findTrueLabel(reassignedTree, contentBuilder).map {
      tree => apply(tree, contentBuilder(_))
    }
  }

}
