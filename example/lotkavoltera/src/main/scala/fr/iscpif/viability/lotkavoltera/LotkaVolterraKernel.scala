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

package fr.iscpif.viability.lotkavoltera

import fr.iscpif.viability._
import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import scala.util.Random
import fr.iscpif.kdtree.visualisation._
import scalax.io._

object LotkaVolterraKernel extends App
    with ViabilityKernel
    with ZoneInput
    with ParallelEvaluator
    with RandomSampler {

  val time = 1.0

  def k(p: Point) = p.forall(c => c >= 8 && c <= 25)

  def dynamic(p: Point) = LotkaVoltera(p(0), p(1), time)

  def zone =
    Seq(
      (0.0, 30.0),
      (0.0, 30.0)
    )

  def depth = 12

  def dimension = 2

  implicit lazy val rng = new Random(42)

  val bassin = apply.get

  println(bassin.volume)

}
