/*
 * Copyright (C) 27/05/13 Romain Reuillon
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
import scala.util.Random

trait Example extends KdTreeComputation {

  case class Content(testPoint: Point, label: Boolean) extends Label with TestPoint

  type T = Content

  implicit def relabel(c: Content, label: Boolean) = c.copy(label = label)

  def contentBuilder(p: Point) = Content(p, oracle(p))
  def oracle(p: Point): Boolean

  def originalTree =
    Tree(
      Leaf(
        Content(point, label = true),
        Zone(zone)
      ),
      depth
    )

  def zone: Seq[Interval]
  def point: Point
  def depth: Int

  def run(implicit rng: Random) = compute(originalTree)

}
