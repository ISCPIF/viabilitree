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
import scala.util.Random
import fr.iscpif.kdtree.content._

trait ViabilityKernel extends ViabilityContent {

  def K(p: Point): Boolean
  def root: Zone
  def depth: Int
  def dynamic(p: Point): Point
  def lipschitz: Option[Double] = None

  def apply(implicit rng: Random) = {

  }

  /*def initialTree(implicit rng: Random) = {

    Tree[Content]()

  }*/

  def sameVolume[T <: Label](t1: Tree[T], t2: Tree[T]) =
    t1.volume == t2.volume

}
