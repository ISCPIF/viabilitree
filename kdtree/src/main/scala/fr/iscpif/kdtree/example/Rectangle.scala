/*
 * Copyright (C) 08/07/13 Romain Reuillon
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

import fr.iscpif.kdtree.algorithm.{ ZoneInput, OracleApproximation }
import fr.iscpif.kdtree.structure._
import scala.math._

object Rectangle extends App with OracleApproximation with ZoneInput {

  def oracle(p: Point) = p.forall(c => c >= -1.2 && c <= 0.8)

  def zone =
    Seq(
      (-2.0, 2.0),
      (-2.0, 2.0),
      (-2.0, 2.0)
    )

  def depth = 15

  val res = apply.get

  println("Nb leaves " + res.leaves.size)
  println("Nb atomic leaves " + res.atomicLeaves.size)
  println("Nb true atomic leaves " + res.atomicLeaves.filter(_.content.label).size)
  println("Volume " + res.volume)

  val dilated = res.dilate

  println("Nb atomic leaves " + dilated.atomicLeaves.size)
  println("Nb true atomic leaves " + dilated.atomicLeaves.filter(_.content.label).size)
  println("Volume dilaté " + dilated.volume)

  val eroded = res.erode

  println("Nb atomic leaves " + eroded.atomicLeaves.size)
  println("Nb true atomic leaves " + eroded.atomicLeaves.filter(_.content.label).size)
  println("Volume érodé " + eroded.volume)
}

