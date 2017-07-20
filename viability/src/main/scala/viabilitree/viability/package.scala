/*
 * Copyright (C) 2014 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


package viabilitree

import viabilitree.kdtree._

import scala.collection.immutable.NumericRange

package object viability {

  implicit class IteratorExtension[A](i: Iterator[A]) {
    def last = {
      def last(i: Iterator[A]): A = {
        val e = i.next()
        if (i.hasNext) last(i)
        else e
      }
      last(i)
    }
    def lastWithTrace(trace: (A, Int) => Unit) = {
      def lastWithTrace(i: Iterator[A], step: Int): A = {
        val e = i.next()
        trace(e, step)
        if (i.hasNext) lastWithTrace(i, step + 1)
        else e
      }
      lastWithTrace(i, 0)
    }
  }


  def volume[T](tree: Tree[T])(implicit c: ContainsLabel[T]) = tree.volume(c.label)

  lazy val Zone = viabilitree.kdtree.Zone
//TODO we need to construct the n-fold Cartesian product, transpose works with collections of same size ONLY since it is the computation of the DIAGONAL
  implicit def vectorOfNumericRangeToVectorOfControl(v: Vector[NumericRange[Double]]) =
    (_: Vector[Double]) => v.transpose.map(Control(_))

  implicit def vectorOfVectorToVectorOfControl(v: Vector[Vector[Double]]) =
    (_: Vector[Double]) => v.map(Control(_))


  type Control = model.Control
  lazy val Control = model.Control

  implicit def indexedSeqToVector[T](i: IndexedSeq[T]) = i.toVector

  def sameVolume[CONTENT](label: CONTENT => Boolean)(t1: Tree[CONTENT], t2: Tree[CONTENT]) =
    t1.volume(label) == t2.volume(label)

}
