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


package fr.iscpif

import fr.iscpif.kdtree.structure.Tree
import fr.iscpif.viability.control._

package object viability {

  implicit class IteratorExtension[A](i: Iterator[A]) {
    def last = {
      def last(i: Iterator[A]): A = {
        val e = i.next()
        if (i.hasNext) last(i)
        else e
      }
      last(i)
    }
    def lastWithTrace(trace: (A, Int) => Unit) = {
      def lastWithTrace(i: Iterator[A], step: Int): A = {
        val e = i.next()
        trace(e, step)
        if (i.hasNext) lastWithTrace(i, step + 1)
        else e
      }
      lastWithTrace(i, 0)
    }
  }

  type ViabilityTree = Tree[ControlledDynamicContent.Content]
}
