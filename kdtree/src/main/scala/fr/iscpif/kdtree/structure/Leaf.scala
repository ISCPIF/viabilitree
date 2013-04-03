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

import fr.iscpif.kdtree.structure.Path._



// TODO: covariant/contravariant problem.
// If we use Leaf[+T] (covariant) then there is a problem: T is contravariant if it occurs as a parameter of a function
trait Leaf[T] extends Node[T] { leaf =>


  def content: T

  //val testPoint: Point
  //val control: Option[Double]

  //def content: Boolean = if (control == None) false else true

  def containingLeaf(point: Point): Option[Leaf[T]] = if (zone.contains(point)) Some(this) else None


  // This function is specific to the bounded case. The output
  // is an Option[Int] that gives the coordinate corresponding to the direction
  def touchesBoundary: Option[Int] = {
    val path = this.reversePath
    val range: Range = this.zone.region.indices
    val coordinate = range.find(coordinate => extremeDivisions(path, coordinate))
    coordinate

  }

  def borderLeaves(direction: Direction): Iterable[Leaf[T]] = List(this)


  //TODO: Define refinable by coordinate: the maxDepth could be different for each coordinate. But think before about convergence
  def refinable(maxDepth: Int) = {
    //zoneVolume(leaf.zone) > 1 / pow(2, maxDepth)
    this.path.length <= maxDepth
  }



  def leaves: Iterable[Leaf[T]] = List(this)



  ///////// REFINING METHODS


  def insert(extendedPath: Path, content: T): Node[T] = {
    assert(extendedPath.length == 1)

    val coordinate = extendedPath.last.coordinate

    val newFork = new Fork[T] {
      val divisionCoordinate = extendedPath.last.coordinate
      val zone: Zone = leaf.zone
    }
    newFork.parent = this.parent

    newFork.parent match {
      case None =>
      case Some(parentValue) => parentValue.descendantType(this) match {
        case Descendant.Low => parentValue.attachLow(newFork)
        case Descendant.High => parentValue.attachHigh(newFork)
        case Descendant.NotDescendant => throw new RuntimeException("The original leaf should be lowChild or HighChild")
      }
    }

    val lowZone: Zone = newFork.zone.divideLow(coordinate)
    val highZone: Zone = newFork.zone.divideHigh(coordinate)


    def generateChild(parentFork: Fork[T], _zone: Zone, _content: T) = new Leaf[T] {
      parent = Some(parentFork)
      val zone =  _zone
      val content = _content

    }

    newFork.attachLow(generateChild(newFork, lowZone, content))
    newFork.attachHigh(generateChild(newFork, highZone, content))

    newFork.rootCalling



  }






  ///////DEBUG HELPERS
  def consistency = true



}