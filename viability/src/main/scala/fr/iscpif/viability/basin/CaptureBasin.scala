package fr.iscpif.viability.basin

import fr.iscpif.kdtree.structure._
import scala.util.Random
import fr.iscpif.kdtree.content._
import fr.iscpif.kdtree.algorithm._
import fr.iscpif.viability.KdTreeComputationForDynamic
import scala.Predef._
import scala.Some
import fr.iscpif.viability.control.ExhaustiveControlTesting

/*
 * Copyright (C) 10/10/13 Isabelle Alvarez
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
trait CaptureBasin <: KdTreeComputationForDynamic with ExhaustiveControlTesting {

  def zone: Zone

  def target(p: Point): Boolean

  def pointInTarget: Point

  def depth: Int

  def shouldBeReassigned(c: CONTENT): Boolean = !c.label

  def defined(p: Point): Boolean = true

  override def viableFunction(tree: Tree[CONTENT]) =
    p => defined(p) && tree.label(p)

  def learnTarget(implicit rng: Random): Option[Tree[CONTENT]] = {
    //def initialContentBuilder(p: Point) = Content(p, None, None, defined(p) && zone.contains(p), 0)

    def initialTree(contentBuilder: Point => CONTENT)(implicit rng: Random, m: Manifest[CONTENT]): Tree[CONTENT] =
      Tree(
        Leaf(
          contentBuilder(align(pointInTarget)),
          zone
        ),
        depth
      )

    def contentBuilder(p: Point) = Content(p, None, None, target(p), 0)

    val learntTarget = learnBoundary(initialTree(contentBuilder), contentBuilder)
    if (learntTarget.leaves.exists(l => l.content.label)) Some(learntTarget) else None
  }

  override def findTrueLabel(t: Tree[CONTENT], contentBuilder: Point => CONTENT)(implicit rng: Random, m: Manifest[CONTENT]): Option[Tree[CONTENT]] = Some(t)

  def apply(implicit rng: Random, m: Manifest[CONTENT]): Iterator[Tree[CONTENT]] = trees

  def trees(implicit rng: Random, m: Manifest[CONTENT]): Iterator[Tree[CONTENT]] = {
    def tree = learnTarget

    Iterator.iterate(tree -> false) {
      case (tree, _) =>
        tree match {
          case None =>
            None -> true
          case Some(tree) =>
            val newTree = timeStep(tree)
            newTree match {
              case None =>
                None -> true
              case Some(nt) =>
                val stop = sameVolume(nt, tree)
                newTree -> stop
            }
        }
    }.takeWhile { case (_, stop) => !stop }.flatMap { case (t, _) => t }
  }

  def sameVolume[T <: Label](t1: Tree[T], t2: Tree[T]) = t1.volume == t2.volume

}
