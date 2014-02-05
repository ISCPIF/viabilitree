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

trait KdTreeComputation extends Sampler with Evaluator with Content {

  type CONTENT <: Label with TestPoint

  def apply(tree: Tree[CONTENT], contentBuilder: Point => CONTENT)(implicit rng: Random, m: Manifest[CONTENT]): Tree[CONTENT] =
    learnBoundary(tree, contentBuilder)

  def learnBoundary(tree: Tree[CONTENT], contentBuilder: Point => CONTENT)(implicit rng: Random, m: Manifest[CONTENT]): Tree[CONTENT] = {
    def refine(tree: Tree[CONTENT]): Tree[CONTENT] = {
      import mutable._

      val leavesToRefine = tree.leavesToRefine(tree)

      if (leavesToRefine.isEmpty) tree
      else
        refine(
          tree.evaluateAndInsert(
            tree.root.zonesAndPathsToTest(leavesToRefine),
            evaluator(contentBuilder)
          )
        )
    }

    refine(tree.clone)
  }

  def findTrueLabel(t: Tree[CONTENT], contentBuilder: Point => CONTENT)(implicit rng: Random, m: Manifest[CONTENT]): Option[Tree[CONTENT]] = {

    // TODO implement lazy computations of leaves
    if (t.leaves.exists(l => l.content.label)) Some(t)
    else {
      val newT = t.clone
      import mutable._

      val leaves =
        newT.leaves.
          filterNot(newT.isAtomic).
          toSeq.sortBy(_.path.length).
          reverse

      // TODO refine is sequential maybe costly if kernel is empty, refine all bigest leaves in parallel?
      // TODO heuristic guess of control?
      def refineNonAtomicLeaves(l: List[Leaf[CONTENT]], tree: Tree[CONTENT]): Option[Tree[CONTENT]] =
        l match {
          case Nil => None
          case l @ (h1 :: _) =>
            val (bigLeaves, smallLeaves) = l.partition(_.path.length == h1.path.length)

            def divide(toDivide: List[Leaf[CONTENT]], divided: List[Leaf[CONTENT]], tree: Tree[CONTENT]): (List[Leaf[CONTENT]], Tree[CONTENT], Boolean) =
              toDivide match {
                case Nil => (divided, tree, false)
                case h2 :: tail =>
                  val divisionCoordinate = h2.minimalCoordinates.head
                  val zptt @ (zone, path) = h2.emptyExtendedZoneAndPath(divisionCoordinate)
                  val newT = tree.evaluateAndInsert(Seq(zptt), evaluator(contentBuilder))
                  val leaf = newT.leaf(path)
                  val label = leaf.getOrElse(sys.error("Leaf should be present in the tree")).content.label
                  if (label) (h2 :: divided, newT, true)
                  else divide(tail, h2 :: divided, tree)
              }

            val (divided, tree, found) = divide(bigLeaves, List.empty, newT)
            if (found) Some(tree)
            else if (!smallLeaves.isEmpty) refineNonAtomicLeaves(divided ::: smallLeaves, tree)
            else refineNonAtomicLeaves(divided.filterNot(tree.isAtomic), tree)
        }
      refineNonAtomicLeaves(leaves.toList, newT)
    }
  }

}
