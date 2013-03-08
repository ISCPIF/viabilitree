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





package fr.iscpif.viability

import fr.iscpif.viability.kdtree.Node._
import util.Random




package object kdtree {
  type Point = Array[Double]
  type IndicatorFunction = Point => Boolean

  //TODO: Specific to the language model. Modify
  // This function is supposed to provide the control (in option) that makes the point (i.e. state) being labelled as true.
  // The label will be deduced from Some or None
  type RichIndicatorFunction = Point => Option[Double]

  type Path = Seq[PathElement]
  case class PathElement(coordinate: Int, descendant: Descendant.Descendant)
  case class Interval(min: Double, max: Double) {
    assume(min < max)
    def span: Double = max - min
    def normalizedSpan(referenceSpan: Double) = span/referenceSpan
  }




  //////////////////// REFINING

  def pairsToSet(pairs: List[(Leaf, Leaf, Int)]): List[(Leaf, Int)] =
    pairs.flatMap {
      case (l1, l2, i) => List(l1 -> i, l2 -> i)
    }

  // The node together with the preferred coordinate (if another coordinate is bigger it will have no impact)
  def nodesToRefine(node: Node, depth: Int): List[(Leaf, Int)] = {
    val leaves =
      (node match {
        case leaf: Leaf => {
          leaf.label match {
            case true => leaf.touchesBoundary match {
              case Some(coordinate) => List((leaf, coordinate))
              case None => List.empty
            }
            case false => List.empty
          }
        }
        case fork: Fork =>
          nodesToRefine(fork.lowChild, depth) ++ nodesToRefine(fork.highChild, depth) ++
            pairsToSet(pairsBetweenNodes(fork.lowChild, fork.highChild))
      }).filter {
        case (leaf, _) => leaf.refinable(depth)
      }

    //TODO: Consider the lines below
    var auxLeaves: List[(Leaf, Int)] = Nil
    leaves.foreach(
      x => {
        if (auxLeaves.forall(y => y._1 != x._1)) auxLeaves = x :: auxLeaves
    }
    )
    auxLeaves
    /*  Source of non-deterministic behaviour
    leaves.groupBy {
      case (l, _) => l
    }.toList.map {
      case (_, l) => l.head
    }
    */
  }


  def attachToLeaf(leaf: Leaf, preferredCoordinate: Int, iFunction: RichIndicatorFunction, rng: Random): Node = {
    //TODO: Delete. Debug
    //println("attachToLeaf  " + leaf.volumeKdTree + " " + preferredCoordinate )

    //?? TODO: Delete?
    assert(leaf.zone.contains(leaf.testPoint))

    //compute the coordinate to split and the span of the zone in this coordinate
    val divisionsOnPreferredCoordinate: Int = leaf.numberOfDivisionsInCoordinate(preferredCoordinate)
    val range: Range = leaf.zone.region.indices
    val optionCoordinate = range.find(c => leaf.numberOfDivisionsInCoordinate(c) < divisionsOnPreferredCoordinate)
    val coordinate =
      optionCoordinate match {
        case None => preferredCoordinate
        case Some(c) => c
      }

    def generateChild(parentFork: Fork, _zone: Zone, _testPoint: Point, _control: Option[Double]) = new Leaf {
      parent = Some(parentFork)
      val zone =  _zone
      val testPoint =  _testPoint
      val control =  _control
    }

    val newFork = new Fork {
      val divisionCoordinate = coordinate
      val zone: Zone = leaf.zone
    }

    newFork.parent = leaf.parent

    newFork.parent match {
      case None =>
      case Some(parentValue) => parentValue.descendantType(leaf) match {
        case Descendant.Low => parentValue.attachLow(newFork)
        case Descendant.High => parentValue.attachHigh(newFork)
        case Descendant.NotDescendant => throw new RuntimeException("The original leaf should be lowChild or HighChild")
      }
    }

    val lowZone: Zone = newFork.zone.divideLow(coordinate)
    val highZone: Zone = newFork.zone.divideHigh(coordinate)

    val lowTestPoint: Point =
      if (lowZone.contains(leaf.testPoint)) leaf.testPoint
      else lowZone.randomPoint(rng)

    val highTestPoint: Point =
      if (highZone.contains(leaf.testPoint)) leaf.testPoint
      else highZone.randomPoint(rng)

    val lowControl: Option[Double] =
      if (lowZone.contains(leaf.testPoint)) leaf.control
      else iFunction(lowTestPoint)

    val highControl: Option[Double] =
      if (highZone.contains(leaf.testPoint)) leaf.control
      else iFunction(highTestPoint)

    newFork.attachLow(generateChild(newFork, newFork.zone.divideLow(coordinate), lowTestPoint, lowControl))
    newFork.attachHigh(generateChild(newFork, newFork.zone.divideHigh(coordinate), highTestPoint, highControl))

    newFork.rootCalling
  }





  // TODO: Change name to distinguish bounded and unbounded
  // computation of kdTree approximation. BOUNDED VERSION
  def kdTreeComputation(initialNode: Node, maxDepth: Int, iFunction: RichIndicatorFunction, rng: Random): Node = {//(implicit rng: Random): Node = {
//    val rootZone = initialNode.zone
    var leavesToRefine = nodesToRefine(initialNode, maxDepth)
    var outputRoot = initialNode
    while (!leavesToRefine.isEmpty) {
      leavesToRefine.foreach(
        leafAndCoord =>
          outputRoot = attachToLeaf(leafAndCoord._1, leafAndCoord._2, iFunction, rng)
      )
      //TODO: Delete. Debug
      //println(testPointsInLeaves(outputRoot))

      leavesToRefine = nodesToRefine(outputRoot, maxDepth)
    }
    outputRoot
  }

  //TODO: Delete. Debug
  def testPointsInLeaves(node: Node): List[List[Double]] = {
    node match{
      case leaf: Leaf => List(leaf.testPoint.toList)
      case fork: Fork => testPointsInLeaves(fork.lowChild) ::: testPointsInLeaves(fork.highChild)
    }
  }






}
