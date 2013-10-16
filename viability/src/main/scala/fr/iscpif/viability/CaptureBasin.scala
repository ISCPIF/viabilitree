package fr.iscpif.viability

import fr.iscpif.kdtree.structure._
import scala.util.Random
import fr.iscpif.kdtree.content._
import fr.iscpif.kdtree.algorithm._

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
trait CaptureBasin extends KdTreeComputationForDynamic with ControlledDynamicContent with Input {

  def zone: Zone

  def inputPoint: Point

  def target(p: Point): Boolean

  def depth: Int

  def shouldBeReassigned(c: CONTENT): Boolean = !c.label

  def learnTarget(tree: Tree[CONTENT])(implicit rng: Random) = {
    def contentBuilder(p: Point) = Content(p, None, None, target(p), 0)
    learnBoundary(tree, contentBuilder)
  }
  protected[viability] def k(p: Point): Boolean = zone.contains(p)

  def initialContentBuilder(p: Point) = Content(p, None, None, k(p), 0)

  override def initialTree(contentBuilder: Point => CONTENT)(implicit rng: Random, m: Manifest[CONTENT]): Option[Tree[CONTENT]] =
    Some(
      Tree(
        Leaf(
          contentBuilder(sampler(zone, rng)),
          zone
        ),
        depth
      )
    )

  override def findTrueLabel(t: Tree[CONTENT], contentBuilder: Point => CONTENT)(implicit rng: Random, m: Manifest[CONTENT]): Option[Tree[CONTENT]] = {
    Some(t)
  }

  def apply(implicit rng: Random, m: Manifest[CONTENT]): Iterator[Tree[CONTENT]] = trees

  def trees(implicit rng: Random, m: Manifest[CONTENT]): Iterator[Tree[CONTENT]] = {
    def tree =
      initialTree(initialContentBuilder).map(learnTarget)
    assert(tree match {
      case Some(t) => (t.leaves.exists(l => l.content.label))
      case None => false
    }, "Bad Target")

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
