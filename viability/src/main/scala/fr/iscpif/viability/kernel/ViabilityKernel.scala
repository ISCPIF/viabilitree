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

package fr.iscpif.viability.kernel

import fr.iscpif.kdtree.content._

import fr.iscpif.kdtree.structure._
import fr.iscpif.model.Control
import scala.util.Random
import fr.iscpif.kdtree.content._
import fr.iscpif.kdtree.algorithm._
import fr.iscpif.viability.TreeRefinement
import fr.iscpif.viability.Domain
import fr.iscpif.viability.control.{ControlledDynamicContent, MemorisedControlTesting}
import fr.iscpif.model

trait ViabilityKernel <: TreeRefinement with Tree0 with MemorisedControlTesting with Domain with FindTrueLabel  with ZoneAttribute { viability =>
/* TODO there is no viability kernel without K, so here kdTreeComputation should be a TreeHandling that can access to K attributes */
  lazy val kdTreeComputation =
    new KdTreeComputation with ErosionInDomain {
      override def buildContent(point: Point, label: Boolean): CONTENT = viability.buildContent(point, label)
      override def label = viability.label
      override type CONTENT = viability.CONTENT
      override def sampler(z: Zone, rng: Random): Point = viability.sampler(z, rng)
      override def domain = viability.domain
     }

  override def findTrueLabel(t: Tree[CONTENT], contentBuilder: (Point) => CONTENT)(implicit rng: Random): Option[Tree[CONTENT]] = kdTreeComputation.findTrueLabel(t, contentBuilder)

  def shouldBeReassigned(c: CONTENT): Boolean = c.label

  def apply()(implicit rng: Random): Iterator[Tree[CONTENT]] = trees

  def trees(implicit rng: Random): Iterator[Tree[CONTENT]] = {
    Iterator.iterate(tree0 -> false) {
      case (tree, _) =>
        tree match {
          case None => None -> true
          case Some(tree) =>
            val newTree = timeStep(tree)
            newTree match {
              case None => None -> true
              case Some(nt) => newTree -> finished(nt, tree)
            }
        }
    }.takeWhile { case (_, stop) => !stop }.flatMap { case (t, _) => t }
  }

  def finished[T <: Label](t1: Tree[T], t2: Tree[T]) = t1.volume == t2.volume

  def domain = zone

}
