/*
 * Copyright (C) 05/07/13 Romain Reuillon
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
import fr.iscpif.kdtree.content._
import monocle.Lenser
import monocle.Macro._

trait ControlledDynamicContent {

  case class Content(
    testPoint: Point,
    control: Option[Int],
    resultPoint: Option[Point],
    label: Boolean,
    controlMax: Int) extends TestPoint with Control with Label

  def buildContent(point: Point, label: Boolean): CONTENT =
    Content(point, None, None, label, 0)

  def label = Lenser[CONTENT](_.label)

  type CONTENT = Content
}

