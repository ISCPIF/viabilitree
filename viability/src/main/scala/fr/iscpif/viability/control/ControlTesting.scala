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

package fr.iscpif.viability.control

import fr.iscpif.kdtree.structure._
import fr.iscpif.viability._

trait ControlTesting <: Dynamic with Content with ControlledDynamicContent {

  def controls: Seq[Control]

  def findViableControl(content: CONTENT, viable: Point => Boolean, tree: Tree[CONTENT]): CONTENT

  def exhaustiveFindViableControl(point: Point, viable: Point => Boolean): CONTENT = {
    val viableControls =
      controls.view.zipWithIndex.map {
        case (control, index) => index -> dynamic(point, control(point))
      }.find {
        case (_, result) => viable(result)
      }

    viableControls match {
      case Some((index, resultPoint)) =>
        ControlledDynamicContent.Content(point, Some(index), Some(resultPoint), true, index)
      case None =>
        notViable(point)
    }
  }

  def notViable(point: Point) = ControlledDynamicContent.Content(point, None, None, false, controls.size)

}
