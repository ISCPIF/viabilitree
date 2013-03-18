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

// TODO: covariant/contravariant problem.
// If we use Fork[+T] (covariant) then there is a problem: T is contravariant if it occurs as a parameter of a function
trait Fork[T] extends Node[T] { fork =>

   val divisionCoordinate: Int

    protected var _lowChild: Node[T] = null
    protected var _highChild: Node[T] = null

    def childrenDefined: Boolean = _lowChild != null && _highChild != null

    def descendantType(child: Node[T]): Descendant.Descendant = {
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









  def leaves: Iterable[Leaf[T]] =  lowChild.leaves ++ highChild.leaves


  def borderLeaves(direction: Direction): Iterable[Leaf[T]] =
    divisionCoordinate match {
      case direction.coordinate =>
        if (direction.sign == Positive) highChild.borderLeaves(direction)
        else lowChild.borderLeaves(direction)
      case _ => lowChild.borderLeaves(direction).toList ::: highChild.borderLeaves(direction).toList
    }



  ///////// REFINING METHODS

  def insert(extendedPath: Path, content: T): Node[T] = {
    assert(extendedPath.length == 1 + path.length && extendedPath(0).coordinate == divisionCoordinate)

     extendedPath(0).descendant match {
       case Descendant.Low => lowChild.insert(extendedPath.drop(1), content)
       case Descendant.High => highChild.insert(extendedPath.drop(1), content)
       case _ => throw new RuntimeException("The path should only contain \'Low\' or \'High\'. ")
     }
  }






  ////// DEBUG HELPERS
  def isParentOfChildren: Boolean = {
    _lowChild.parent == Some(this) && _highChild.parent == Some(this)
  }

  def consistency: Boolean = {
    this.childrenDefined && this.isParentOfChildren &&
    this.lowChild.consistency && this.highChild.consistency &&
    this.lowChild.isChildOfParent && this.highChild.isChildOfParent
  }
  ///////////







}
