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


package fr.iscpif.viability.kdtree


trait Fork extends Node {
   val divisionCoordinate: Int

    protected var _lowChild: Node = null
    protected var _highChild: Node = null

    def childrenDefined: Boolean = _lowChild != null && _highChild != null

    def descendantType(child: Node): Descendant.Descendant = {
      if (_lowChild == child) Descendant.Low
      else if (highChild == child) Descendant.High
      else Descendant.NotDescendant
    }

    def attachLow(child: Node) {
      _lowChild = child
      child.parent = Some(this)
    }

    def attachHigh(child: Node) {
      _highChild = child
      child.parent = Some(this)
    }

    def containingLeaf(point: Point): Option[Leaf] =
      if (!zone.contains(point)) None
      else lowChild.containingLeaf(point) orElse highChild.containingLeaf(point)

    def lowChild = if (childrenDefined) _lowChild else throw new RuntimeException("Children are not defined. (1)")

    def highChild = if (childrenDefined) _highChild else throw new RuntimeException("Children are not defined. (2)")


  def leafExtractor: List[Leaf] = lowChild.leafExtractor ::: highChild.leafExtractor


  def volumeKdTree: Double = lowChild.volumeKdTree + highChild.volumeKdTree

  def volumeKdTreeNormalized(referenceZone: Zone): Double =
    lowChild.volumeKdTreeNormalized(referenceZone) + highChild.volumeKdTreeNormalized(referenceZone)

  def leaves: List[Leaf] =  lowChild.leaves ++ highChild.leaves


  def borderLeaves(direction: Direction, label: Boolean): List[Leaf] =
    divisionCoordinate match {
      case direction.coordinate =>
        if (direction.sign == Positive) highChild.borderLeaves(direction, label)
        else lowChild.borderLeaves(direction, label)
      case _ => lowChild.borderLeaves(direction, label) ::: highChild.borderLeaves(direction, label)
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
