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
import fr.iscpif.kdtree.algorithm._

trait ViabilityKernel extends KdTreeComputationForDynamic with ViabilityContent with Input {

  def k(p: Point): Boolean
  def dynamic(p: Point): Point

  def initialContentBuilder(p: Point): CONTENT = buildContent(p, dynamic(p), k(p))

  def apply(implicit rng: Random, m: Manifest[CONTENT]): Option[Tree[CONTENT]] = {
    def step(tree: Tree[CONTENT], s: Int = 0): Option[Tree[CONTENT]] = {
      val newT = apply(tree)
      newT match {
        case None => None
        case Some(t) =>
          endOfStep(s, t)

          if (sameVolume(t, tree)) Some(t)
          else step(t, s + 1)
      }
    }

    initialTree.
      map(apply(_, initialContentBuilder(_))).
      flatMap(step(_))
  }

  def sameVolume[T <: Label](t1: Tree[T], t2: Tree[T]) =
    t1.volume == t2.volume

  def endOfStep(s: Int, t: Tree[CONTENT]) = {}

}
