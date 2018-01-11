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

package viabilitree.viability

import viabilitree.kdtree._
import viabilitree.approximation._

import scala.util.Random

object treeRefinement {

  def exhaustiveFindViableControl[CONTENT](
    point: Vector[Double],
    viable: Vector[Double] => Boolean,
    buildContent: (Vector[Double], Option[Int], Option[Vector[Double]], Boolean, Int) => CONTENT,
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    controls: Vector[Double] => Vector[Control]): CONTENT = {

    val ctrls = controls(point)
    val viableControls =
      ctrls.view.zipWithIndex.map {
        case (control, index) => index -> dynamic(point, control.value)
      }.find {
        case (_, result) => viable(result)
      }

    def notViable(point: Vector[Double]) = buildContent(point, None, None, false, ctrls.size)

    viableControls match {
      case Some((index, resultPoint)) => buildContent(point, Some(index), Some(resultPoint), true, index)
      case None => notViable(point)
    }
  }

  // Number of dilation can be coded in dilate function
  // Was timestep
  def refine[CONTENT: Manifest](
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    controls: Vector[Double] => Vector[Control],
    shouldBeReassigned: CONTENT => Boolean,
    findViableControl: (CONTENT, Vector[Double] => Boolean, NonEmptyTree[CONTENT]) => CONTENT,
    findTrueLabel: FindTrueLabel[CONTENT],
    learnBoundary: LearnBoundary[CONTENT],
    sampler: Sampler,
    dilate: (NonEmptyTree[CONTENT], Random) => NonEmptyTree[CONTENT],
    buildContent: (Vector[Double], Option[Int], Option[Vector[Double]], Boolean, Int) => CONTENT,
    label: CONTENT => Boolean)(tree: NonEmptyTree[CONTENT], rng: Random): Tree[CONTENT] = {

    def viableFunction(t: NonEmptyTree[CONTENT], p: Vector[Double]) = tree.label(label, p)
    def viable(p: Vector[Double], rng: Random) = viableFunction(dilate(tree, rng), p)

    val reassignedTree =
      tree.reassign { content =>
        if (shouldBeReassigned(content)) findViableControl(content, viable(_, rng), tree)
        else content
      }

    //TODO: May want to use (tree => findViableControl) function in order to benefit from optimised heuristic for viable control research.
    def contentBuilder(p: Vector[Double], rng: Random) =
      exhaustiveFindViableControl(p, viable(_, rng), buildContent, dynamic, controls)

    def ev = evaluator.sequential(contentBuilder(_, rng), sampler)

    findTrueLabel(reassignedTree, rng).mapNonEmpty {
      tree => learnBoundary(tree, ev, rng)
    }
  }

}
//
//trait TreeRefinement <: Model with ControlTesting with ControlledDynamicContent { refine =>
//
//  def kdTreeComputation: KdTreeComputation {  type CONTENT = refine.CONTENT  }
//
//  def dimension: Int
//
//  def lipschitz: Option[Double] = None
//
//  def dilations: Int =
//    lipschitz match {
//      case Some(l) => (floor(l * sqrt(dimension) / 2) + 1).toInt
//      case None => 0
//    }
//
//  def shouldBeReassigned(c: CONTENT): Boolean
//
//  def viableFunction(tree: NonEmptyTree[CONTENT]) = tree.label(_)
//
//  def timeStep(tree: NonEmptyTree[CONTENT])(implicit rng: Random): Option[NonEmptyTree[CONTENT]] = {
//    val viable = viableFunction(kdTreeComputation.dilate(tree, dilations))
//
//    val reassignedTree =
//      tree.reassign( content => if (shouldBeReassigned(content)) findViableControl(content, viable, tree) else content )
//
//    //TODO: May want to use (tree => findViableControl) function in order to benefit from optimised heuristic for viable control research.
//    def contentBuilder(p: Point) = exhaustiveFindViableControl(p, viable)
//
//    kdTreeComputation.findTrueLabel(reassignedTree, contentBuilder).map {
//      tree => kdTreeComputation.learnBoundary(tree, contentBuilder)
//    }
//  }
//
//  def erodeInDomain(t: NonEmptyTree[CONTENT])(implicit rng: Random): NonEmptyTree[CONTENT] = kdTreeComputation.erode(t)
//  def erodeInDomain(t: NonEmptyTree[CONTENT], n: Int)(implicit rng: Random): NonEmptyTree[CONTENT] = kdTreeComputation.erode(t,n)
//
//  def apply(tree: NonEmptyTree[CONTENT])(implicit rng: Random, m: Manifest[CONTENT]): Option[NonEmptyTree[CONTENT]] = timeStep(tree)
//}
//
