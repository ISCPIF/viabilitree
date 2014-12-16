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

package fr.iscpif.kdtree.algorithm

import scala.util.Random
import fr.iscpif.kdtree.structure._

// TODO new subtrait for non rectangular inputs
trait Input extends Content {
  /** *
    *
    * Return a tree containing at least a point with a true label
    *
    * @param contentBuilder
    * @param rng
    * @param m
    * @return
    */
  def initialTree(contentBuilder: Point => CONTENT)(implicit rng: Random, m: Manifest[CONTENT]): Option[Tree[CONTENT]]
}
