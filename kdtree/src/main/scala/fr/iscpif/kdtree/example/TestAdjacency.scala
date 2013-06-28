/*
 * Copyright (C) 03/04/13 Romain Reuillon
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
import math._

object TestAdjacency extends App {

  // val path1: Path = List(PathElement(3,Descendant.Low),PathElement(2,Descendant.Low),PathElement(4,Descendant.Low))
  // val path2: Path = List(PathElement(3,Descendant.High),PathElement(2,Descendant.Low),PathElement(4,Descendant.High))
  // edge en dim 3 ou 4 passe

  val path1: Path = List(PathElement(3, Descendant.High), PathElement(2, Descendant.Low), PathElement(4, Descendant.Low))
  val path2: Path = List(PathElement(3, Descendant.Low), PathElement(2, Descendant.Low), PathElement(4, Descendant.High))
  // le pb apparaît qd on intercale une dim semblable entre 2 inversion.

  // val path1: Path = List(PathElement(3,Descendant.Low),PathElement(4,Descendant.Low))
  // val path2: Path = List(PathElement(3,Descendant.High),PathElement(4,Descendant.High))

  // val path1: Path = List(PathElement(1,Descendant.Low),PathElement(3,Descendant.Low),PathElement(2,Descendant.Low),PathElement(4,Descendant.Low))
  // val path2: Path = List(PathElement(1,Descendant.Low),PathElement(3,Descendant.High),PathElement(2,Descendant.High),PathElement(4,Descendant.High))
  // coin en dim 4

  // val path1: Path = List(PathElement(3,Descendant.Low),PathElement(2,Descendant.Low),PathElement(4,Descendant.Low))
  // val path2: Path = List(PathElement(3,Descendant.High),PathElement(2,Descendant.High),PathElement(4,Descendant.High))
  // coin en dim3   ne passe pas

  println("Adjacent paths (edges) " + Path.adjacent(path1, path2))

}
