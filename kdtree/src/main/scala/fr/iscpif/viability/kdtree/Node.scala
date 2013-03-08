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

import fr.iscpif.viability.kdtree.HelperFunctions._
import fr.iscpif.viability.kdtree.Path._

import com.rits.cloning.Cloner
import util.Random


object Node {


  // The critical pairs together with the coordinate of adjacency (no need to include the sign)
  def pairsBetweenNodes(node1: Node, node2: Node): List[(Leaf, Leaf, Int)] = {
    val direction =
      adjacency(node1.path, node2.path) match {
        case None => throw new RuntimeException("Zones must be adjacent.")
        case Some(x) => x.conversionToDirection
      }

    (node1, node2) match {
      case (leaf1: Leaf, leaf2: Leaf) =>
        if (xor(leaf1.label, leaf2.label))
          List((leaf1, leaf2, direction.coordinate))
        else
          Nil

      case (fork1: Fork, fork2: Fork) =>
        val listAux = List(
          (fork1.lowChild, fork2.lowChild),
          (fork1.lowChild, fork2.highChild),
          (fork1.highChild, fork2.lowChild),
          (fork1.highChild, fork2.highChild))
        def functAux(nodes: (Node, Node)) = {
          val (n1, n2) = nodes
          if (adjacent(n1.path, n2.path)) (pairsBetweenNodes(n1, n2))
          else Nil
        }
        listAux.flatMap(functAux)

      case (fork: Fork, leaf: Leaf) =>
        val listAux: List[Leaf] = fork.borderLeaves(direction, (!leaf.label))
        val list: List[(Leaf, Leaf, Int)] = listAux.map(borderLeaf => (leaf, borderLeaf, direction.coordinate))
        list.filter(x => adjacent(x._1.path, x._2.path))

      case (leaf: Leaf, fork: Fork) =>
        val listAux: List[Leaf] = fork.borderLeaves(direction.opposite, (!leaf.label))
        val list: List[(Leaf, Leaf, Int)] = listAux.map(borderLeaf => (leaf, borderLeaf, direction.coordinate))
        list.filter(x => adjacent(x._1.path, x._2.path))
    }
  }

  def diff(n1: Node, n2: Node) =
    n1.leaves.count(l => !n2.contains(l.testPoint))




}



trait Node {

  var parent: Option[Fork] = None
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

  def rootCalling: Node = parent match {
    case None => this
    case Some(parent) => parent.rootCalling
  }

  def contains(point: Point) = containingLeaf(point).isDefined

  def containingLeaf(point: Point): Option[Leaf]

  // It is supposed to be applied on the root
  def preferredDirections: List[Direction] = {
    assert(this.parent == None)
    def preferableDirection(direction: Direction): Boolean = if (this.borderLeaves(direction, true) != Nil) true else false
    val positiveDirections: List[Direction] = this.zone.region.indices.toList.map(c => new Direction(c, Positive))
    val negativeDirections: List[Direction] = this.zone.region.indices.toList.map(c => new Direction(c, Negative))
    val preferredPDirections = positiveDirections.filter(d => preferableDirection(d))
    val preferredNDirections = negativeDirections.filter(d => preferableDirection(d))
    preferredPDirections ::: preferredNDirections
  }

  def leafExtractor: List[Leaf]

  def labelledLeafExtractor(label: Boolean): List[Leaf] =
    this.leafExtractor.filter(l => l.label == label)

  def volumeKdTree: Double

  def volumeKdTreeNormalized(referenceZone: Zone): Double

  def leaves: List[Leaf]

  // TODO: Delete??
  /*
  def clone = {
    val cloner = new Cloner
    cloner.deepClone(this)
  }
  */

  def borderLeaves(direction: Direction, label: Boolean): List[Leaf]

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



  ////////// DEBUG HELPERS
  def isChildOfParent: Boolean = {
    parent match {
      case None => throw new RuntimeException("This node is supposed to have a parent.")
      case Some(fork) => fork.lowChild == this || fork.highChild == this
    }
  }

  def consistency: Boolean


  def pathSoundness(node: Node): Boolean =
    node match {
      case leaf: Leaf => true
      case fork: Fork => fork.lowChild.path.last.coordinate == fork.highChild.path.last.coordinate &&
        pathSoundness(fork.lowChild) && pathSoundness(fork.highChild)
    }

  def printPaths(node: Node) {
    node match {
      case leaf: Leaf => println(leaf.path); println("BRANCH END")
      case fork: Fork => println(fork.path); printPaths(fork.lowChild); printPaths(fork.highChild)
    }
  }

  /////////////



}