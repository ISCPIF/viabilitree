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

package fr.iscpif.kdtree.structure

import com.rits.cloning.Cloner

import scala.reflect.ClassTag

object Tree {

  def apply[T](content: T, zone: Zone, depth: Int): Tree[T] =
    apply(Leaf(content, zone), depth)

  def apply[T](_root: Node[T], _depth: Int): Tree[T] =
    new Tree[T] {
      val root = _root
      val depth = _depth
    }
}

trait Tree[T] {

  def depth: Int
  def root: Node[T]
  def isAtomic(l: Leaf[T]) = l.depth >= depth
  def atomicLeaves = root.leaves.filter(isAtomic)
  def dimension = root.zone.region.size
  def leaf(path: Path) = root.leaf(path)

  // TODO implement lazy computations of leaves, possible thanks to immutable tree
  //def leaves: Iterable[Leaf[T]]

  def clone[T: ClassTag] = {
    val cloner = new Cloner
    cloner.registerImmutable(scala.reflect.classTag[T].runtimeClass)
    cloner.dontCloneInstanceOf(classOf[Descendant.Descendant])
    cloner.dontCloneInstanceOf(None.getClass)
    cloner.deepClone(this)
  }

}
