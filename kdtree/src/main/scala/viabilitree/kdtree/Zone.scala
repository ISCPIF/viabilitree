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

package viabilitree.kdtree

import scala.util.Random

object Zone {

  def apply(intervals: (Double, Double)*): Zone = Zone(intervals.map(x => x: Interval).toVector)

  def apply(intervals: Vector[Interval]): Zone =
    new Zone {
      val region = intervals.toArray
    }
  // TODO pour le test d'adjacence à virer sinon avec le includes de Interval et le equivalence
  def adjacencyDirAux(zone1: Zone, zone2: Zone) = {
    var result: Option[Direction] = None
    var adjacentZones = true
    for (i <- 0 to zone1.region.length - 1) {
      if (!(includes(zone1.region(i), zone2.region(i)) || includes(zone2.region(i), zone1.region(i)))) {
        result match {
          //There is more than one direction where intervals are not included
          case Some(_) => adjacentZones = false
          //test if adjacent intervals:
          case None =>
            if (equivalence(zone1.region(i).max, zone2.region(i).min)) result = Some(Direction(i, Positive))
            else if (equivalence(zone2.region(i).max, zone1.region(i).min)) result = Some(Direction(i, Negative))
            else adjacentZones = false
        }
      }
    }
    if (adjacentZones) result else None
  }

  def adjacentZones(zone1: Zone, zone2: Zone) = {
    adjacencyDirAux(zone1, zone2) match {
      case Some(_) => true
      case None => false
    }
  }

  def center(z: Zone): Vector[Double] =
    z.region.map { i => i.min + (i.max - i.min) / 2 }.toVector

  implicit def seqToZone(zone: Seq[(Double, Double)]) = apply(zone: _*)

  def equals(z1: Zone, z2: Zone) =
    z1.region.deep == z2.region.deep

  def divide(zone: Zone, divisionCoordinate: Int, sign: Sign) =
    sign match {
      case Negative => zone.divideLow(divisionCoordinate)
      case Positive => zone.divideHigh(divisionCoordinate)
    }

}

trait Zone { zone: Zone =>
  //TODO: Consider IndexSeq instead of Vector. Change val to def
  val region: Array[Interval]
  def dimension = region.size

  def divideLow(d: Int): Zone =
    new Zone {
      //TODO: Change def to val
      val region = {
        val aux = (zone.region(d).min + zone.region(d).max) / 2
        val low = Interval(zone.region(d).min, aux)
        zone.region.updated(d, low)
      }
    }

  def divideHigh(d: Int): Zone =
    new Zone {
      //TODO: Change def to val
      val region = {
        val aux = (zone.region(d).min + zone.region(d).max) / 2
        val high = Interval(aux, zone.region(d).max)
        zone.region.updated(d, high)
      }
    }

  def contains(point: Vector[Double]): Boolean =
    (point zip region).forall { case (p, r) => r.min <= p && p < r.max }

  //Draw a random point in a zone
  def randomPoint(rng: Random): Vector[Double] =
    region.map(i => i.min + rng.nextDouble() * ((i.max - i.min))).toVector

  def volume: Double =
    zone.region.foldLeft(1.0)(
      (x, interval) => x * interval.span)

  def normalizedVolume(referenceZone: Zone): Double = {
    val referenceSpans: Array[Double] = referenceZone.region.map(x => x.span)
    val zippedIntervals = zone.region.zip(referenceSpans)
    val normalizedSpans: Array[Double] = zippedIntervals.map(x => x._1.span / x._2)
    def product(x: Double, y: Double): Double = x * y
    normalizedSpans.foldLeft(1.0)(product)
  }

  override def toString = zone.region.mkString(",")
}