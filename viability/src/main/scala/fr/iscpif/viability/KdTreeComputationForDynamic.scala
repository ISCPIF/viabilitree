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
      if (nb >= 0) t
      else dilate(t.dilate, nb - 1)
    dilate(tree, dilations)
  }

  def dilations: Int =
    lipschitz match {
      case Some(l) => (floor(l * sqrt(dimension) / 2) + 1).toInt
      case None => 0
    }

  def buildContent(from: Point, result: Point, viable: Boolean): CONTENT

  def apply(tree: Tree[CONTENT])(implicit rng: Random, m: Manifest[CONTENT]): Option[Tree[CONTENT]] = {
    val dilated = dilatedTree(tree)

    def viable(p: Point): Boolean =
      dilated.root.containingLeaf(p).map(_.content.label).getOrElse(false)

    def contentBuilder(p: Point): CONTENT = {
      val result = dynamic(p)
      buildContent(p, result, viable(result))
    }

    def reassignTree(implicit relabel: Relabeliser[CONTENT]) =
      tree.reassign(
        t =>
          if (t.label) relabel(t, t => viable(t.result))
          else t
      )

    val reassignedTree = reassignTree
    findViablePoint(reassignedTree, contentBuilder).map(tree => super.apply(tree, contentBuilder))
  }

  def findViablePoint(t: Tree[CONTENT], contentBuilder: Point => CONTENT)(implicit rng: Random, m: Manifest[CONTENT]): Option[Tree[CONTENT]] = {

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
                  val zptt = h2.emptyExtendedZoneAndPath(divisionCoordinate)
                  val newT = tree.evaluateAndInsert(Seq(zptt), evaluator(contentBuilder))
                  val leaf = newT.leaf(zptt._2)
                  if (leaf.map(_.content.label).getOrElse(false)) (h2 :: divided, tree, true)
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
