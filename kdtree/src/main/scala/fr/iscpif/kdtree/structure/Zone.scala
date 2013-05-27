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

package fr.iscpif.kdtree.structure

import scala.util.Random

object Zone {
  def apply(intervals: Interval*) =
    new Zone {
      val region = intervals.toArray
    }
}

trait Zone {
  zone: Zone =>
  //TODO: Consider IndexSeq instead of Vector. Change val to def
  val region: Array[Interval]

  def divideLow(d: Int): Zone =
    new Zone {
      //TODO: Change def to val
      val region = {
        val aux = (zone.region(d).min + zone.region(d).max) / 2
        val low = new Interval(zone.region(d).min, aux)
        zone.region.updated(d, low)
      }
    }

  def divideHigh(d: Int): Zone =
    new Zone {
      //TODO: Change def to val
      val region = {
        val aux = (zone.region(d).min + zone.region(d).max) / 2
        val high = new Interval(aux, zone.region(d).max)
        zone.region.updated(d, high)
      }
    }

  def contains(point: Point): Boolean = (point zip region).forall {
    case (p, r) => r.min <= p && p < r.max
  }

  //Draw a random point in a zone
  def randomPoint(rng: Random): Point =
    region.map(i => i.min + rng.nextDouble() * ((i.max - i.min)))

  def volume: Double = {
    def auxFunc(x: Double, interval: Interval) = x * interval.span
    zone.region.foldLeft(1.0)(auxFunc)
  }

  def normalizedVolume(referenceZone: Zone): Double = {
    val referenceSpans: Array[Double] = referenceZone.region.map(x => x.span)
    val zippedIntervals = zone.region.zip(referenceSpans)
    val normalizedSpans: Array[Double] = zippedIntervals.map(x => x._1.span / x._2)
    def product(x: Double, y: Double): Double = x * y
    normalizedSpans.foldLeft(1.0)(product)

  }

  //TODO: Delete? Debug
  override def toString = region.toString()

}