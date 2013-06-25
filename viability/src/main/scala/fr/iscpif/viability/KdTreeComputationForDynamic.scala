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

trait KdTreeComputationForDynamic extends KdTreeComputation {

  case class Content(testPoint: Point, result: Point, label: Boolean) extends Label with TestPoint
  implicit def relabel(c: Content, label: Boolean) = c.copy(label = label)

  type T = Content

  def dynamic(p: Point): Point
  def viable(p: Point): Boolean =
    dilatedTree.root.containingLeaf(p).map(_.content.label).getOrElse(false)

  lazy val dilatedTree = {
    def dilate(t: Tree[T], nb: Int): Tree[T] =
      if (nb >= 0) t
      else dilate(t.dilate, nb - 1)
    dilate(currentTree, dilations)
  }

  def dilations: Int
  def currentTree: Tree[T]
  def mu: Double

  def contentBuilder(p: Point): T = {
    val result = dynamic(p)
    Content(p, result, viable(result))
  }

}
