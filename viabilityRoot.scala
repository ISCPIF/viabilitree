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

// TODO: Using bounded version, should we change to the unbounded one?

import fr.iscpif.viability.kdtreeUnbounded._
import fr.iscpif.viability.initialSteps.InputProcessing._
//import fr.iscpif.cabbage._
import fr.iscpif.viability.LanguageModel._


import math._
import Function._
import scala.util.Random


// Most parts of this KdtreeBounded are specific to the unbounded case
package object viabilityRoot {//extends App {

  //I'd like to use dependent types: "State" would be the type "Seq[Double] of length stateDimension", etc.
  //type State = Array[Double]
  //type Control = Array[Double]
  //type StateControl = (State, Control)
  //type Model = StateControl => State
  //type IndicatorFunction = State => Boolean

  trait MainMethods {
    val stateDimension: Int
    val controlDimension: Int
    // The number of slices of the capture basin
    val numberOfSteps: Int
    val maxDepth: Int
    val stateInTarget: State
    val randomNG = new Random(3)


    def initialPointGuesser(model: Model, iFunction: IndicatorFunction, searchStateZone: Zone, searchControlZone: Zone)
                           (implicit rng: Random): (State, Control) = {
      assume(searchStateZone.region.length == stateDimension && searchControlZone.region.length == controlDimension)
      var initialPointFound = false
      // We may be interested in monitoring the number of guesses
      var numberOfGuesses = 0
      var guessPoint: (State, Control) = null
      while (!initialPointFound) {
        guessPoint = (randomPoint(searchStateZone), randomPoint(searchControlZone).apply(0))
        val image = model(guessPoint._1, guessPoint._2)
        numberOfGuesses += 1
        if (iFunction(image) == true) initialPointFound = true
      }
      guessPoint
    }

    def initialNodeFromStateControl(point: (State,Control)): Node = {
      throw new NotImplementedError("It needs to be implemented")
    }
    def initialNodeFromState(point: State): Node = {
      throw new NotImplementedError("It needs to be implemented")
    }
    def searchStateZoneEstimation: Zone = {
      throw new NotImplementedError("It needs to be implemented")
    }
    def searchControlZoneEstimation: Zone = {
      throw new NotImplementedError("It needs to be implemented")
    }




    //Each node of the output list is the root of the kdtree which represents the corresponding slice of the capture tube.
    //The first node corresponds to the target, the last one to the capture basin at Tmax
    def captureTubeStateControl(numberOfSteps: Int, model: Model, target: IndicatorFunction): List[Node] = {
      numberOfSteps match {
        case 0 => {
          val initNode = initialNodeFromState(stateInTarget)
          List(kdTreeComputation(initNode, maxDepth, target)(randomNG))
        }
        case n => {
          val precedingTube = captureTubeStateControl(n - 1, model, target)
          def indFunction: IndicatorFunction = {x: State => precedingTube.last.isInKdTree(x) }
          val stateZone = searchStateZoneEstimation
          val controlZone = searchControlZoneEstimation
          val initStateControl = initialPointGuesser(model, indFunction, stateZone, controlZone)(randomNG)
          val initNode = initialNodeFromStateControl(initStateControl)
          precedingTube ::: List(kdTreeComputation(initNode, maxDepth, indFunction)(randomNG))
        }
      }
    }


    //The same as captureTube, but the "slices" are calculated dynamically
    def lightCaptureTubeStateControl(model: Model, target: IndicatorFunction): Int => Node = {
      n =>
        val initNode = initialNodeFromState(stateInTarget)
        var currentSlice: Node = kdTreeComputation(initNode, maxDepth, target)(randomNG)
        var i = 0
        while (i < n) {
          def indFunction: IndicatorFunction = x => currentSlice.isInKdTree(x)
          val stateZone = searchStateZoneEstimation
          val controlZone = searchControlZoneEstimation
          val initStateControl = initialPointGuesser(model, indFunction, stateZone, controlZone)(randomNG)
          val initNode = initialNodeFromStateControl(initStateControl)
          currentSlice = kdTreeComputation(initNode, maxDepth, indFunction)(randomNG)
          i += 1
        }
        currentSlice
    }


  }


  //////////// TEST MAIN

  /*
  override def main(args: Array[String]) {
    println("Hello, world!")
    //Main.plotResults()
    //println(TestInputs.treeSet.contains(TestInputs.p1))
    //val closestInGrid = InitialSteps.FirstKdTree.closestGridPoint(TestInputs.stateTest)
    //println("Closest in grid: " + closestInGrid + " " +  TestInputs.projectionTreeSet.contains(closestInGrid))
    //println("Volume: " + Test.volumeSolution + "  " + "Max Depth: " + Test.depth)
  }
  */

}


