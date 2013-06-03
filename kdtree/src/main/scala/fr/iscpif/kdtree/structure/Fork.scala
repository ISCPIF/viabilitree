/*
 * Copyright (C) 2013 de Aldama / Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
published by
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

trait Fork[T] extends Node[T] { fork =>

  val divisionCoordinate: Int

  protected var _lowChild: Node[T] = null
  protected var _highChild: Node[T] = null

  def childrenDefined: Boolean = _lowChild != null && _highChild != null

  def reassignChild(from: Node[T], to: Node[T]) =
    descendantType(from) match {
      case Descendant.Low => attachLow(to)
      case Descendant.High => attachHigh(to)
      case Descendant.NotDescendant => throw new RuntimeException("The original leaf should be lowChild or highChild")
    }

  def descendantType(child: Node[_]): Descendant.Descendant = {
    if (_lowChild == child) Descendant.Low
    else if (highChild == child) Descendant.High
    else Descendant.NotDescendant
  }

  def attachLow(child: Node[T]) {
    _lowChild = child
    child.parent = Some(this)
  }

  def attachHigh(child: Node[T]) {
    _highChild = child
    child.parent = Some(this)
  }

  def containingLeaf(point: Point): Option[Leaf[T]] =
    if (!zone.contains(point)) None
    else lowChild.containingLeaf(point) orElse highChild.containingLeaf(point)

  def lowChild = if (childrenDefined) _lowChild else throw new RuntimeException("Children are not defined. (1)")

  def highChild = if (childrenDefined) _highChild else throw new RuntimeException("Children are not defined. (2)")

  def leaves: Iterable[Leaf[T]] = lowChild.leaves ++ highChild.leaves

  def borderLeaves(direction: Direction): Iterable[Leaf[T]] =
    divisionCoordinate match {
      case direction.coordinate =>
        direction.sign match {
          case Positive => highChild.borderLeaves(direction)
          case Negative => lowChild.borderLeaves(direction)
        }
      case _ => lowChild.borderLeaves(direction).toList ::: highChild.borderLeaves(direction).toList
    }

  def replace(path: Path, content: T): Node[T] =
    path.toList match {
      case Nil =>
        val newLeaf = Leaf[T](content, zone)
        parent match {
          case None => newLeaf
          case Some(p) =>
            p reassignChild (this, newLeaf)
            newLeaf.rootCalling
        }
      case h :: t =>
        h.descendant match {
          case Descendant.Low => lowChild.replace(t, content)
          case Descendant.High => highChild.replace(t, content)
          case _ => sys.error("Descendant child expected")
        }
    }

  ///////// REFINING METHODS

  def insert(extendedPath: Path, content: T): Node[T] = {
    assert(extendedPath(0).coordinate == divisionCoordinate)

    extendedPath(0).descendant match {
      case Descendant.Low => lowChild.insert(extendedPath.drop(1), content)
      case Descendant.High => highChild.insert(extendedPath.drop(1), content)
      case _ => throw new RuntimeException("The path should only contain \'Low\' or \'High\'. ")
    }
  }

  def isParentOfChildren: Boolean = {
    _lowChild.parent == Some(this) && _highChild.parent == Some(this)
  }

}
