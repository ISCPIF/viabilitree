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


package fr.iscpif.viability.kernel

import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import fr.iscpif.viability.K
import scala.util.Random

trait  ZoneK <: ViabilityKernel with K with Input {
  def zone: Zone
  override def domain = zone
  /* par défaut, le domaine est K dans le cas de la zone hyperrectangle. Quand ce n'est pas le cas il faut redéfinir domain */

  override def k(p: Point): Boolean = zone.contains(p)

  override def tree0(implicit rng: Random): Option[Tree[CONTENT]] = {
    def contentBuilder(p: Point) = exhaustiveFindViableControl(p, k)
    initialTree(contentBuilder).map {
      tree => kdTreeComputation.learnBoundary(tree, contentBuilder)
    }
  }
}
