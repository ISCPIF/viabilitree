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

import fr.iscpif.kdtree.structure.Node._
import fr.iscpif.kdtree.structure.Node
import fr.iscpif.kdtree.structure.Fork
import fr.iscpif.kdtree.structure.Leaf

import label.Label
import util.Random




package object structure {
  type Point = Seq[Double]
  type IndicatorFunction = Point => Boolean


  type Path = Seq[PathElement]
  case class PathElement(coordinate: Int, descendant: Descendant.Descendant)
  case class Interval(min: Double, max: Double) {
    assume(min < max)
    def span: Double = max - min
    def normalizedSpan(referenceSpan: Double) = span/referenceSpan
  }




  //////////////////// REFINING

  def pairsToSet[T](pairs: Iterable[(Leaf[T], Leaf[T], Int)]): Iterable[(Leaf[T], Int)] =
    pairs.flatMap {
      case (l1, l2, i) => List(l1 -> i, l2 -> i)
    }

















}
