/*
 * Copyright (C) 03/04/13 Romain Reuillon
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

package fr.iscpif.kdtree.example

import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.content._
import math._
import fr.iscpif.kdtree.algorithm._

object Circle extends App with OracleApproximation with ZoneInput {

  def oracle(p: Point) =
    pow(p(0), 2) + pow(p(1), 2) + pow(p(2), 2) <= pow(1, 3)

  def zone =
    Seq(
      (-2.0, 2.0),
      (-2.0, 2.0),
      (-2.0, 2.0)
    )

  def point = Seq(0.0, 0.0, 0.0)

  def depth = 12

  val res = apply.get

  println("Nb leaves " + res.leaves.size)
  println("Nb atomic leaves " + res.atomicLeaves.size)
  println("Nb true atomic leaves " + res.atomicLeaves.filter(_.content.label).size)
  println("Volume " + res.volume)

  val dilated = dilate(res)

  println("Nb atomic leaves " + dilated.atomicLeaves.size)
  println("Nb true atomic leaves " + dilated.atomicLeaves.filter(_.content.label).size)
  println("Volume dilaté " + dilated.volume)

  val eroded = erode(res)

  println("Nb atomic leaves " + eroded.atomicLeaves.size)
  println("Nb true atomic leaves " + eroded.atomicLeaves.filter(_.content.label).size)
  println("Volume érodé " + eroded.volume)
}
