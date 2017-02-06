/*
 * Copyright (C) 07/07/13 Romain Reuillon
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

package viabilitree.kdtree.algorithm

import viabilitree.kdtree.structure._
import scala.util.Random

object Sampler {
  //TODO do it nicely from path instead of zone
  //TODO use superclass sampler method if possible
  def grid(depth: Int, zone: Zone, dimension: Int) = new Sampler {

    private def numberOfDivision: Int = depth / dimension
    private def numberCells: Int = powOf2(numberOfDivision)
    private def powOf2(p: Int) = 1 << p

    assert(depth % dimension == 0, "Depth should be a multiple of dimension")

    def apply(z: Zone, rng: Random): Vector[Double] = align(z.randomPoint(rng))

    override def align(p: Vector[Double]) = cellNumberToGridPoint(cellNumbers(p))

    def cellNumbers(point: Vector[Double]) = {
      (point zip zone.region).map {
        case (coord, interval) =>
          (numberCells * ((coord - interval.min) / interval.span)).floor.toInt match {
            case x if x < 0 => 0
            case x if x > numberCells - 1 => numberCells - 1
            case x => x
          }
      }
    }

    def cellNumberToGridPoint(cellNumber: Vector[Int]): Vector[Double] =
      (cellNumber zip zone.region).map {
        case (number, interval) => (interval.span / numberCells) * (number + 0.5) + interval.min
      }

  }

  def random = new Sampler {
    override def apply(z: Zone, rng: Random): Vector[Double] = z.randomPoint(rng)
    def align(p: Vector[Double]): Vector[Double] = p
  }
}

// TODO change structure. It should be the generalization of both GridSampler and RandomSampler
trait Sampler {
  def apply(z: Zone, rng: Random): Vector[Double]
  def align(p: Vector[Double]): Vector[Double]
}

