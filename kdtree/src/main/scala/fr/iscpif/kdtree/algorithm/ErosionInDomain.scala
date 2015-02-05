/*
 * Copyright (C) 2015 Romain Reuillon
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


package fr.iscpif.kdtree.algorithm

import fr.iscpif.kdtree.structure.{Zone, Leaf, Tree}

import scala.util.Random

trait ErosionInDomain <: Erosion { self: KdTreeComputation =>

  def domain: Zone

  override def erode(t: Tree[CONTENT], additionalLeaves: Seq[Leaf[CONTENT]])(implicit rng: Random): Tree[CONTENT] = {
    val leavesOnBorderWithDomain = computeOnBorderWithDomain(t)
    super.erode(t, additionalLeaves ++ leavesOnBorderWithDomain)
  }

  def computeOnBorderWithDomain(t: Tree[CONTENT]): Iterable[Leaf[CONTENT]]= {
    t.leavesOnRootZone(t).filter {
      case(leaf,i) => borderOnDomain(leaf,i)
    }.map {
      n => n._1
    }
  }

  /* Warning leaf is supposed to be atomic
   */

  def borderOnDomain(leaf: Leaf[CONTENT], i: Int): Boolean = {
    val aux = (leaf.zone.region(i).max - leaf.zone.region(i).min) / 2
    val a = leaf.zone.region(i).min
    val minDomain = domain.region(i).min
    val b = leaf.zone.region(i).max
    val maxDomain = domain.region(i).max
    (a > minDomain + aux)&& (b < maxDomain - aux)
  }

}
