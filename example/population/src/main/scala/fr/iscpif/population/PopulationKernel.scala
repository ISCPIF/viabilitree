/*
 * Copyright (C) 30/09/13 Isabelle Alvarez
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

package fr.iscpif.population

import fr.iscpif.viability._
import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.content._
import fr.iscpif.kdtree.visualisation._
import scala.util.Random
import scalax.io.Resource
import math._

object PopulationKernel extends App with OracleApproximation with ZoneAndPointInput {
  val a = 0.2
  val b = 3.0
  val c = 0.5
  val d = -2.0
  val e = 2.0
  /*
for these parameters, for every value of x in [a,b], there is a value of y such that (x,y) is in the kernel
 */
  def oracle(p: _root_.fr.iscpif.kdtree.structure.Point): Boolean = {
    p(0) >= a && p(0) <= b &&
      p(1) >= -sqrt(2 * c * log(b / p(0))) && p(1) <= sqrt(2 * c * log(p(0) / a))
  }

  def zone: Zone = Seq((a, b), (d, e))

  def point = Seq(1.0, 0.0)

  def depth: Int = 20

  apply.get.saveVTK2D(Resource.fromFile(s"/tmp/population/kernelVT${depth}.vtk"))

}
