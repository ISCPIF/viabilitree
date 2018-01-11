/*
 * Copyright (C) 14/07/13 Romain Reuillon
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

package viabilitree.model

trait Model {

  var integrationStep: Double

  var timeStep: Double
  // TODO dynamic with timeStep and integrationStep as variables
  //  def dynamic(point: Vector[Double], control: Vector[Double], timeStep: Double = timeStep, integrationStep: Double=integrationStep): Vector[Double]

  def dynamic(point: Vector[Double], control: Vector[Double]): Vector[Double]

  /*  override def strategy(p: Point): Point = {
    if !label(p) Nil
    // si le point n'est pas dans le noyau de viabilité c'est qu'il y a une erreur !
    else {
      val leafP = containingLeaf(p)
      val u = leafP[content].control
      // on veut le contrôle de la feuille qui contient p quand p est viable
    }
  }*/

  def trajectory(p: Vector[Double], c: Vector[Double] => Vector[Double], i: Int): List[Vector[Double]] = {
    if (i == 0) List(p)
    else p :: trajectory(dynamic(p, c(p)), c, i - 1)
  }

  // def perturbation(state: Vector[Double], s: Double): Vector[Double] => Boolean
}