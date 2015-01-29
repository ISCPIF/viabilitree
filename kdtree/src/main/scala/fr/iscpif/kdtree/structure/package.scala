/*
 * Copyright (C) 2013 de Aldama / Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
published by
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

package fr.iscpif.kdtree

import language.implicitConversions

package object structure {
  type Point = fr.iscpif.geometry.Point
  type Path = Seq[PathElement]
  case class PathElement(coordinate: Int, descendant: Descendant.Descendant)

  implicit def tupleToInterval(t: (Double, Double)) = {
    val (min, max) = t
    Interval(min, max)
  }

  implicit def intervalsToZone(intervals: Seq[(Double, Double)]) =
    Zone(
      intervals.map { case (min, max) => Interval(min, max) }
    )

  case class Interval(min: Double, max: Double) {
    assume(min < max)
    def span: Double = max - min
    def normalizedSpan(referenceSpan: Double) = span / referenceSpan
  }
  // TODO pour le test d'adjacence A virer sinon

  def includes(a: Interval, b: Interval) = {
    assert(a.min < a.max && b.min < b.max)
    (a.min < b.min || equivalence(a.min, b.min)) && (a.max > b.max || equivalence(a.max, b.max))
  }

  def equivalence(a: Double, b: Double): Boolean = {
    val eps = 10e-10
    if (a == 0) (b.abs <= eps)
    else if (b == 0) (a.abs <= eps)
    else (a - b).abs <= (a.abs + b.abs) * eps
  }

  //////////////////// REFINING

  def pairsToSet[T](pairs: Iterable[(Leaf[T], Leaf[T], Int)]): Iterable[(Leaf[T], Int)] =
    pairs.flatMap {
      case (l1, l2, i) => List(l1 -> i, l2 -> i)
    }

  implicit def treeToNode[T](t: Tree[T]) = t.root

}
