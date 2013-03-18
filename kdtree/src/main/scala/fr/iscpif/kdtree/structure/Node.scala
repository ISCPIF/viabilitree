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

import fr.iscpif.kdtree.structure.HelperFunctions._
import fr.iscpif.kdtree.structure.Path._
import fr.iscpif.kdtree.structure._


import com.rits.cloning.Cloner
import util.Random


object Node {

  //TODO: Add testPoint in content!!
  //def diff(n1: Node, n2: Node) =
  //  n1.leaves.count(l => !n2.contains(l.testPoint))




}



trait Node[T] { node =>

  var parent: Option[Fork[T]] = None
  val zone: Zone

  def path: Path = reversePath.reverse

  lazy val reversePath: Path = parent match {
    case None => Seq.empty
    case Some(parent) => {
      parent.descendantType(this) match {
        case Descendant.Low => PathElement(parent.divisionCoordinate, Descendant.Low) :: parent.reversePath.toList
        case Descendant.High => PathElement(parent.divisionCoordinate, Descendant.High) :: parent.reversePath.toList
        case Descendant.NotDescendant =>
          throw new RuntimeException("The node must be Low(child) or High(child) of its parent. (2)")
      }
    }
  }

  def isRoot: Boolean = if (parent == None) true else false

  def rootCalling: Node[T] = parent match {
    case None => this
    case Some(parent) => parent.rootCalling
  }

  def contains(point: Point) = containingLeaf(point).isDefined

  def containingLeaf(point: Point): Option[Leaf[T]]

  def leaves: Iterable[Leaf[T]]

  // TODO: Delete??
  /*
  def clone = {
    val cloner = new Cloner
    cloner.deepClone(this)
  }
  */

  def borderLeaves(direction: Direction): Iterable[Leaf[T]]

  def numberOfDivisionsInCoordinate(coordinate: Int): Int = {
    val divisionCoordinates: Seq[Int] = reversePath.map( x => x.coordinate)
    divisionCoordinates.filter( x => x == coordinate).size
  }

  //Only needed for the unbounded version
  //TODO: Use this for refine function
  // It chooses the direction to expand a node (it will be a initialNode)
  def chooseDirection(preferredDirections: List[Direction], rng: Random): Direction = {
    val spanList: List[(Double, Int)] = zone.region.map(i => i.max - i.min).toList.zipWithIndex
    val smallestSpans: List[(Double, Int)] = spanList.filter(k => spanList.forall(i => k._1 <= i._1))
    val smallestCoordinates: List[Int] = smallestSpans.map(x => x._2)
    val selectedDirections = preferredDirections.filter(k => smallestCoordinates.exists(i => i == k.coordinate))
    if (selectedDirections != Nil) randomElement(selectedDirections, rng)
    else {
      val direction = randomElement(smallestCoordinates, rng)
      val sign = if (rng.nextBoolean()) Positive else Negative
      new Direction(direction, sign)
    }
  }


  ///////// REFINING METHODS

  def insert(extendedPath: Path, content: T): Node[T]



  ////////// DEBUG HELPERS
  def isChildOfParent: Boolean = {
    parent match {
      case None => throw new RuntimeException("This node is supposed to have a parent.")
      case Some(fork) => fork.lowChild == this || fork.highChild == this
    }
  }

  def consistency: Boolean


  def pathSoundness(node: Node[T]): Boolean =
    node match {
      case leaf: Leaf[T] => true
      case fork: Fork[T] => fork.lowChild.path.last.coordinate == fork.highChild.path.last.coordinate &&
        pathSoundness(fork.lowChild) && pathSoundness(fork.highChild)
    }

  def printPaths(node: Node[T]) {
    node match {
      case leaf: Leaf[T] => println(leaf.path); println("BRANCH END")
      case fork: Fork[T] => println(fork.path); printPaths(fork.lowChild); printPaths(fork.highChild)
    }
  }

  /////////////



}