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

package viabilitree.kdtree.structure

import viabilitree.kdtree.structure.Path._

object Leaf {

  def apply[T](_content: T, _zone: Zone) =
    new Leaf[T] {
      val zone = _zone
      val content = _content
    }

}

trait Leaf[T] extends Node[T] { self =>

  def content: T
  def containingLeaf(point: Vector[Double]): Option[Leaf[T]] = if (zone.contains(point)) Some(this) else None

  // This function is specific to the bounded case. The output
  // is an Option[Int] that gives the coordinate corresponding to the direction
  def touchesBoundary: Option[Int] = {
    val path = reversePath
    val range: Range = zone.region.indices
    val coordinate = range.find(coordinate => extremeDivisions(path, coordinate))
    coordinate
  }

  def borderLeaves(direction: Direction): Iterable[Leaf[T]] = Vector(this)

  def refinable(maxDepth: Int) = path.length < maxDepth

  def leaves: Iterable[Leaf[T]] = Vector(this)

  def replace(path: Path, content: T): Node[T] = {
    assert(path.isEmpty)
    parent match {
      case None => Leaf[T](content, zone)
      case Some(p) =>
        val newLeaf = Leaf[T](content, zone)
        newLeaf.parent = Some(p)
        p.reassignChild(this, newLeaf)
        newLeaf
    }
  }

  def insert(extendedPath: Path, content: T) = {
    assert(extendedPath.length == 1, s"$extendedPath should be of length 1")

    val coordinate = extendedPath.last.coordinate
    val descendant = extendedPath.last.descendant

    val newFork = new Fork[T] {
      val divisionCoordinate = extendedPath.last.coordinate
      val zone: Zone = self.zone
    }
    newFork.parent = this.parent

    newFork.parent match {
      case None =>
      case Some(parentValue) => parentValue.reassignChild(this, newFork)
    }

    val lowZone: Zone = newFork.zone.divideLow(coordinate)
    val highZone: Zone = newFork.zone.divideHigh(coordinate)

    def generateChild(parentFork: Fork[T], _zone: Zone, _content: T) = new Leaf[T] {
      parent = Some(parentFork)
      val zone = _zone
      val content = _content
    }

    if (descendant == Descendant.Low) {
      newFork.attachLow(generateChild(newFork, lowZone, content))
      newFork.attachHigh(generateChild(newFork, highZone, self.content))
      newFork
    } else if (descendant == Descendant.High) {
      newFork.attachLow(generateChild(newFork, lowZone, self.content))
      newFork.attachHigh(generateChild(newFork, highZone, content))
      newFork
    } else throw new RuntimeException("Descendant should be low or high.")

  }

  def extendedLowPath(coordinate: Int) =
    (PathElement(coordinate, Descendant.Low) :: reversePath.toList).reverse

  def extendedHighPath(coordinate: Int) =
    (PathElement(coordinate, Descendant.High) :: reversePath.toList).reverse

  def minimalCoordinates = {
    val coordCardinals = path.groupBy(_.coordinate).map { case (k, v) => k -> v.size }
    val allCoords = (0 until zone.dimension).map { d => coordCardinals.getOrElse(d, 0) }
    val minCardinal = allCoords.min
    allCoords.zipWithIndex.filter { case (c, _) => c == minCardinal }.map { case (_, i) => i }.sorted
  }

  def leaf(path: Path): Option[Leaf[T]] =
    if (path.isEmpty) Some(this)
    else None

  /* def touches(leaf: Leaf[T]): Boolean = {
   val dim = this.zone.region.length
   var test1 = this.zone.region
   var test2 = leaf.zone.region
   var adjacent  = true

 }*/

}