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

import fr.iscpif.viability.kdtreeBounded._
import fr.iscpif.viability.initialSteps.InputProcessing._
import fr.iscpif.viability.viabilityRoot._
import fr.iscpif.viability.initialSteps._
import fr.iscpif.viability.LanguageModel._

import scalax.io._
import scalax.file.Path
import scalax.io.StandardOpenOption._
import math._
import Function._
import scala.util.Random


package object viabilityLanguages {

  //TODO: Review!! And create the inputs properly (in a class)
  //INITIAL ARGUMENTS

  val dir = "/tmp/logKdTree/"
  val stateDimension = 3
  val controlDimension = 1
  val maxDepth = 18
  val numberOfControlTests = 20
  val randomNG = new Random(3)
  val timeStep: Double = 1

  val integrationStep: Double = 0.1
  val sampleTimes: Seq[Double] = Seq(0.0, timeStep)
  val model = modelCreation(integrationStep, sampleTimes)


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

  // TODO: Change [epsilon]. Taking it into account may avoid some errors (e.g java.lang.ArrayIndexOutOfBoundsException)
  // Since sDot = uControl(t) and s should belong to [0,1], u must be in the interval1 defined below. [timeStep] is the
  // time interval between two slices. [epsilon] is used to avoid numerical problems: analytically we'll get s\in [0+epsilon, 1-epsilon]
  def validControlInterval(state: State) = {
    val epsilon = pow(10, -10)
    //val interval1 = new Interval((epsilon-state.s)/timeStep, (1-state.s-epsilon)/timeStep)
    val interval1 = new Interval(-state.s / timeStep, (1 - state.s) / timeStep)
    if (interval1.max <= -0.1 || interval1.min >= 0.1 || abs(interval1.max - interval1.min) < epsilon) None
    else Some(Interval(max(interval1.min, -0.1), min(interval1.max, 0.1)))
  }

  def controlTestOrder(interval: Option[kdtreeBounded.Interval]): Array[Double] =
    interval match {
      case None => Array(0.0)
      case Some(intervalValue) =>
        val step = (intervalValue.max - intervalValue.min) / (numberOfControlTests - 1)
        //TODO manage odd numberOfControlTests
        (0 until numberOfControlTests / 2).flatMap(
          n => List(intervalValue.max - n * step, intervalValue.min + n * step)
        ).toArray
    }

  def randomConstrainedPoint(implicit rng: Random): Point = {
    val sigmaA = rng.nextDouble()
    val sigmaB = rng.nextDouble() * (1 - sigmaA)
    val s = rng.nextDouble()
    Array(sigmaA, sigmaB, s)
  }

  // TODO: We may be interested in monitoring the number of guesses
  def initialPointGuesser(targetIFunction: IndicatorFunction)(implicit rng: Random): (State, Control) = {

    var initialPointFound = false
    //The initialisation is meaningless, the values will be immediately overridden
    var guessPoint: State = null
    var guessControl: Double = 0.0
    while (!initialPointFound) {
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
    }
    (guessPoint, guessControl)
  }

  def initialNodeCreation(richTargetIFunction: RichIndicatorFunction)(implicit rng: Random): Node = {
    val targetIFunction = conversionToIndicator(richTargetIFunction)
    val root = new Leaf {

      val stateControl: (State, Double) = initialPointGuesser(targetIFunction)(rng)
      val testPoint: Point = stateControl._1
      val control: Option[Double] = Some(stateControl._2)

      val zone = new Zone {
        val unitInterval = new Interval(0, 1)
        val region: Array[Interval] = Array(unitInterval, unitInterval, unitInterval)
      }
    }
    root
  }

  def targetIFunctionCreation(target: Node, model: Model): RichIndicatorFunction = {
    state: State =>
    //TODO: Should we consider this assumption?
    //state.foreach(x => assume( 0 <= x && x<= 1))
      if (state.sigmaA + state.sigmaB > 1) None
      else controlTestOrder(validControlInterval(state)).find {
        c =>
          val image = model(state, c)
          target.contains(image)
      }

  }


  def captureTube(s: Int)(implicit rng: Random): Node = {
    def outputFile(s: Int) = dir + "kdTree" + s + "depth" + maxDepth + "step" + timeStep + ".csv"

    def initialSlice = {
      val firstSlice = initialSteps.FirstKdTree.firstKdTree
      deleteFile(outputFile(1))
      val output: Output = Resource.fromFile(outputFile(1))
      kdTreeToFile(firstSlice, output)
      println("First slice created.")
      println(volumeKdTree(firstSlice))
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
        println(volumeKdTree(newSlice))
        //println(diff(newSlice, slice) + "," + (diff(slice, newSlice)))
        newSlice
    }
  }

  // OUTPUT FUNCTIONS

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


  //TEST SIMPLE FIGURES
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

    val testPoint: Point = Array(0.0, 0.0, 0.0)
    val control: Option[Double] = Some(0.0)

    val zone = new Zone {
      val interval = new Interval(-1, 1)
      val region: Array[Interval] = Array(interval, interval, interval)
    }
  }

  /*def test(depth: Int) {
    val kdTreeSphere = kdTreeComputation(root, depth, iFunctionSphere())(randomNG)
    val output: Output = Resource.fromFile("./OutputKdTrees/kdTree_Sphere_depth" + depth +".csv")
    kdTreeToFile(kdTreeSphere, output)
  }   */


  /// MAIN
  def produceTrajectory(state: State, u: Control, node: Node)(implicit rng: Random) = {
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


  def main(args: Array[String]) {
    println("Hello world")
    captureTube(40)(randomNG)
    //28, 48, 39
    /*
    val perturbed = Array(22.0 / 99, 54.0 / 99, 39.0 / 99)
    val kernel = FirstKdTree.targetToIFunction
    //println(kernel(perturbed))
    //println(model(perturbed, kernel(perturbed).get).toList)

    def modelCreation(integrationStep: Double, sampleTimes: Seq[Double]) = {
      (state: State, control: Control) =>
        assert(state.length == 3)
        val model = new LanguageModel(state.sigmaA, state.sigmaB, state.s, control)
        model.integrate(sampleTimes, integrationStep)
    }
    val newModel = modelCreation(0.01, (0.0 to 1.0 by 0.1))
    newModel(perturbed, 0.1).foreach {
      case (t, s) => println(s"$t,${s.toList}")

    }
    */

    //initialSteps.projectionTreeSet.contains(closestGridPoint(image))



  }


  /*produceTrajectory(perturbed, 0.0, last)(randomNG).foreach {
    case(s1, s2, c) => println(s"s1 = $s1, s2 = $s2, c = $c")
  }    */


  //val interval = new Interval(0, 19)
  //println(controlTestOrder(interval).toList)


  /*
  // This point is in the first slice: one valid control should lead us to the target
  val state: State = Array(0.71875,0.21875,0.984375)
  val control = 0
  val model = new LanguageModel(state.sigmaA, state.sigmaB, state.s, control)
  val sampleTimes = (0.0 to 0.05 by 0.01)
  val integrationStep = 0.005



  model.integrate(sampleTimes, integrationStep).foreach{case(t, s) =>println(t -> s.toList +
    "this state goes to the target in 0.01 time using control " + initialSteps.FirstKdTree.targetToIFunction()(s).toString())}
  */





}


