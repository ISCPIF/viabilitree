/*
 * Copyright (C) 2013 de Aldama / Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
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
import fr.iscpif.viability.kdtree._
import fr.iscpif.cabbage.Cabbage4ViabilityReduced._
import fr.iscpif.viability.viabilityCabbage.CabbageModelWrap._

import math._
import Function._
import scala.util.Random
import scala.Some
import scalax.io._
import scalax.file.Path
import scalax.io.StandardOpenOption._
import org.apache.commons.math3.random.{RandomAdaptor, Well44497b}


package object viabilityCabbage{

  //TODO: Review! And create the inputs properly (in a class)
  //INITIAL ARGUMENTS

  val dir = "../OutputKdTrees/cabagge"
  val maxDepth = 8
  //The maximal number of tests to find an initial point
  val numberOfGuessingPoints = 10000
  //The maximal number of controls to test, once a state has been fixed
  val numberOfControlTests = 20

  //val randomNG = new Random(3)
  //Measured in minutes
  val timeStep: Double = 5

  val integrationStep: Double = 0.1
  val sampleTimes: Seq[Double] = Seq(0.0, timeStep)
  val model = modelCreation(integrationStep, sampleTimes)

  val explorationZone: Zone = new Zone {
    val mvInterval = new Interval(0, 105)
    val cGLSInVInterval = new Interval(0, 0.22344)
    val TInterval = new Interval(80, 120)
    val ABPInterval = new Interval(0, 5)
    val ABInterval = new Interval(0, 0.6)
    val TextureInterval = new Interval(0, 3)

    val region = Array(mvInterval, cGLSInVInterval, TInterval, ABPInterval, ABInterval, TextureInterval)
  }


  //TODO: Should it be defined elsewhere?
  // TYPES
  type State = Array[Double]
  type Control = Double
  type Model = (State, Control) => State
  type IndicatorFunction = Array[Double] => Boolean
  type RichIndicatorFunction = Array[Double] => Option[Double]

  def conversionToIndicator(rIFunction: RichIndicatorFunction): IndicatorFunction = {
    x: Array[Double] =>
      rIFunction(x) match {
        case Some(_) => true
        case None => false
      }
  }

  //The temperature may vary 1 degree per minute
  def validControlInterval(state: State): Interval = {
      new Interval(state.T - timeStep, state.T + timeStep)
  }

  def randomConstrainedPoint(rng: Random): Point = {
    explorationZone.region.map(x => x.min + rng.nextDouble()*x.span)
  }

  def controlTestOrder(interval: Interval): Array[Double] = {
        val step = (interval.max - interval.min) / (numberOfControlTests - 1)
        //TODO: manage odd numberOfControlTests
        (0 until numberOfControlTests / 2).flatMap(
          n => List(interval.max - n * step, interval.min + n * step)
        ).toArray
    }

  def initialPointGuesser(targetIFunction: IndicatorFunction, rng: Random): (State, Control) = {

    var initialPointFound = false
    //The initialisation is meaningless, the values will be immediately overridden
    var guessPoint: State = null
    var guessControl: Double = 0.0
    var numberOfGuesses: Int = 0
    while (!initialPointFound && numberOfGuesses <= numberOfGuessingPoints) {
      guessPoint = randomConstrainedPoint(rng)
      val guessControlArray = controlTestOrder(validControlInterval(guessPoint))
      var k = 0
      while (k < numberOfControlTests && !initialPointFound) {
        guessControl = guessControlArray(k)
        val image = model(guessPoint, guessControl)
        k += 1
        if (targetIFunction(image) == true) {
          initialPointFound = true
        }
      }
     numberOfGuesses += 1
    }
    initialPointFound match {
      case false => throw new Exception("The number of tested points reached "+ numberOfGuessingPoints)
      case true =>  (guessPoint, guessControl)

    }
  }

  def initialNodeCreation(richTargetIFunction: RichIndicatorFunction, rng: Random): Node = {
    val targetIFunction = conversionToIndicator(richTargetIFunction)
    val root = new Leaf {
      val stateControl: (State, Double) = initialPointGuesser(targetIFunction, rng)
      val testPoint: Point = stateControl._1
      val control: Option[Double] = Some(stateControl._2)
      //Exploration Zone
      val zone: Zone = explorationZone
    }

    root
  }

  def targetIFunctionCreation(target: Node, model: Model): RichIndicatorFunction = {
    state: State =>
      controlTestOrder(validControlInterval(state)).find {
        c =>
          val image = model(state, c)
          target.contains(image)
      }
  }

  /*
  def captureTube(s: Int)(implicit rng: Random): Node = {
    def outputFile(s: Int) = dir + "kdTree" + s + "depth" + maxDepth + "step" + timeStep + ".csv"

    def initialSlice = {
      val firstSlice = FirstKdTree.firstKdTree(rng)
      deleteFile(outputFile(1))
      val output: Output = Resource.fromFile(outputFile(1))
      //TODO: Uncomment
      //kdTreeToFile(firstSlice, output)
      println("First slice created.")
      println(firstSlice.volumeKdTree)
      firstSlice
    }

    (2 to s).foldLeft(initialSlice) {
      (slice, step) =>
        def currentIFunction: RichIndicatorFunction = targetIFunctionCreation(slice, model)
        val initNode = initialNodeCreation(currentIFunction)
        val newSlice = kdTreeComputation(initNode, maxDepth, currentIFunction)(rng)
        deleteFile(outputFile(step))
        val output: Output = Resource.fromFile(outputFile(step))
        kdTreeToFile(newSlice, output)
        println("One slice created.")
        println(newSlice.volumeKdTree)
        //println(diff(newSlice, slice) + "," + (diff(slice, newSlice)))
        newSlice
    }
  }
  */


  //// OUTPUT FUNCTIONS

  def leafZoneToFile(leaf: Leaf, outputResource: Output) {
    //val output: Output = Resource.fromFile(outputFile)
    assert(leaf.zone.region.length == 3)
    val intervals = leaf.zone.region
    val origin = Array(intervals(0).min, intervals(1).min, intervals(2).min)
    val lengthSA = (intervals(0).max - intervals(0).min).toString()
    val lengthSB = (intervals(1).max - intervals(1).min).toString()
    val lengthS = (intervals(2).max - intervals(2).min).toString()
    val line: List[String] =
      List(origin(0).toString(), origin(1).toString(), origin(2).toString(), lengthSA, lengthSB, lengthS)
    //val list: List[String] = List(leaf.)
    outputResource.writeStrings(line, ";")(Codec.UTF8)
    outputResource.write("\n")
  }

  def kdTreeToFile(node: Node, outputResource: Output) {
    node match {
      case leaf: Leaf => if (leaf.label == true) leafZoneToFile(leaf, outputResource)
      case fork: Fork => kdTreeToFile(fork.lowChild, outputResource); kdTreeToFile(fork.highChild, outputResource)
    }
  }

  def deleteFile(pathName: String) {
    val path = scalax.file.Path.fromString(pathName)
    if (path.exists) path.delete(false)
  }


  ////////////TEST SIMPLE FIGURES
  def iFunctionCircle(): Array[Double] => Option[Double] = {
    point: Array[Double] => {
      if (pow(point(0), 2) + pow(point(1), 2) <= 1) Some(0.0)
      else None
    }
  }

  def iFunctionSphere(): Array[Double] => Option[Double] = {
    point: Array[Double] => {
      if (pow(point(0), 2) + pow(point(1), 2) + pow(point(2), 2) <= 1) Some(0.0)
      else None
    }
  }

  def iFunctionSmallCube(): Array[Double] => Option[Double] = {
    point: Array[Double] => {
      if (-0.5 <= point(0) && point(0) <= 0.5 && -0.5 <= point(1) && point(1) <= 0.5 && -0.5 <= point(2) && point(2) <= 0.5) Some(0.0)
      else None
    }
  }
  val root = new Leaf {

    val testPoint: Point = Array(0.0, 0.0)
    val control: Option[Double] = Some(0.0)

    val zone = new Zone {
      val interval = new Interval(-1, 1)
      val region: Array[Interval] = Array(interval, interval)
    }
  }
  /*
  def test(depth: Int) {
   val kdTreeSphere = kdTreeComputation(root, depth, iFunctionSphere())(randomNG)
   val output: Output = Resource.fromFile("./OutputKdTrees/kdTree_Sphere_depth" + depth +".csv")
   kdTreeToFile(kdTreeSphere, output)
  }
  */
  ///////////////////





  /////////////////// MAIN
  //TODO: MODIFY
  /*
  def generateTrajectory(state: State, u: Control, node: Node)(implicit rng: Random) = {
    val kernel = FirstKdTree.targetToIFunction
    //val perturbedState = node.containingLeaf(state).map(_.testPoint).getOrElse(sys.error("State should be in kdTree"))
    Iterator.iterate((state, state, u)) {
      case (_, s, _) =>
        val newState = model(s, u)
        if (node.contains(newState)) (state, newState, u)
        else {
          val leaf = node.containingLeaf(s).getOrElse(sys.error("State should be in kdTree"))
          (leaf.testPoint, model(leaf.testPoint, leaf.control.get), leaf.control.get)
        }
    }.takeWhile {
      case (_, s, _) => !kernel(s).isDefined
    }.toList
  }
  */



  def main(args: Array[String]) {
    println("Hello world")
    //println(FirstKdTree.firstKdTree(new Random(3), new Random(5)).volumeKdTree)
    val depth = 2
    println(
      kdTreeComputation(root, depth, iFunctionCircle(), new Random(3)).volumeKdTree  + "  depth = " + depth)
    //captureTube(12)(randomNG)







  }





}


