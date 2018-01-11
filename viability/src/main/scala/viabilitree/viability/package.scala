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

  // TODO we need to deal with empty list of control
  implicit def vectorOfNumericRangeToVectorOfControl(v: Vector[NumericRange[Double]]) = {

    def cartesianProduct(v: Vector[NumericRange[Double]]): Vector[Vector[Double]] = {

      def cartesianProduct(a: NumericRange[Double], b: NumericRange[Double]) =
        a.toVector.map(p => b.toVector.map(o => Vector(p, o))).flatten

      def cross2WithTail(accumulator: Vector[Vector[Double]], v: Vector[NumericRange[Double]]): Vector[Vector[Double]] = {
        def crossVector(a: Vector[Vector[Double]], b: NumericRange[Double]): Vector[Vector[Double]] =
          a.map(p => b.toVector.map(o => p :+ o)).flatten

        v.length match {
          case 0 => accumulator
          case _ =>
            val t = v.tail
            val hd = v.head
            t.length match {
              case 0 => crossVector(accumulator, hd)
              case _ => cross2WithTail(crossVector(accumulator, hd), t)
            }
        }
      }

      val t = v.tail
      val hd = v.head

      t.length match {
        case 0 => hd.map(p => Vector(p)).toVector
        case _ => cross2WithTail(cartesianProduct(hd, t.head), t.tail)
      }
    }

    (_: Vector[Double]) => cartesianProduct(v).map(Control(_))
  }

  implicit def vectorOfVectorToVectorOfControl(v: Vector[Vector[Double]]) =
    (_: Vector[Double]) => v.map(Control(_))

  type Control = model.Control
  lazy val Control = model.Control

  //(p: Vector[Double]) => ak.contains(viabilitree.viability.kernel.Content.label.get, p)
  def kernelOrBasinToOracle[T](t: Tree[T])(implicit c: ContainsLabel[T]) = {
    (p: Vector[Double]) => t.contains(c.label, p)
  }

  implicit def indexedSeqToVector[T](i: IndexedSeq[T]) = i.toVector

  def sameVolume[CONTENT](label: CONTENT => Boolean)(t1: Tree[CONTENT], t2: Tree[CONTENT]) =
    t1.volume(label) == t2.volume(label)

  lazy val Touch = kdtree.Path.Touch
  lazy val High = Descendant.High
  lazy val Low = Descendant.Low

}
