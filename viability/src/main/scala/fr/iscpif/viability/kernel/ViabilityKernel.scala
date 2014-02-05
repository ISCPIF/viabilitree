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

import fr.iscpif.kdtree.structure._
import scala.util.Random
import fr.iscpif.kdtree.content._
import fr.iscpif.kdtree.algorithm._
import fr.iscpif.viability.{ K, KdTreeComputationForDynamic }
import fr.iscpif.viability.control.MemorisedControlTesting

trait ViabilityKernel <: KdTreeComputationForDynamic
    with Input
    with K
    with MemorisedControlTesting {

  def shouldBeReassigned(c: CONTENT): Boolean = c.label

  def learnConstraintSet(tree: Tree[CONTENT])(implicit rng: Random) = tree

  def apply(implicit rng: Random, m: Manifest[CONTENT]): Iterator[Tree[CONTENT]] = trees

  def trees(implicit rng: Random, m: Manifest[CONTENT]): Iterator[Tree[CONTENT]] = {
    def tree =
      initialTree(exhaustiveFindViableControl(_, k)).map(learnConstraintSet)

    Iterator.iterate(tree -> false) {
      case (tree, _) =>
        tree match {
          case None => None -> true
          case Some(tree) =>
            val newTree = timeStep(tree)
            newTree match {
              case None => None -> true
              case Some(nt) => newTree -> sameVolume(nt, tree)
            }
        }
    }.takeWhile { case (_, stop) => !stop }.flatMap { case (t, _) => t }
  }

  def sameVolume[T <: Label](t1: Tree[T], t2: Tree[T]) = t1.volume == t2.volume

}
