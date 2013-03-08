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
import fr.iscpif.viability.viabilityCabbage.Import._
import fr.iscpif.viability.viabilityCabbage.Export._


import math._
import Function._
import scala.util.Random
import scala.Some
import scalax.io._
import scalax.file.Path
import scalax.io.StandardOpenOption._
import org.apache.commons.math3.random.{RandomAdaptor, Well44497b}
import collection.immutable.TreeSet


package object viabilityCabbage{

  //TODO: Review! And create the inputs properly (in a class)
  //INITIAL ARGUMENTS

  val dir = "../OutputKdTrees/cabbage/"
  val maxDepth = 13
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

  val initializationZone: Zone = new Zone {
    val mvInterval = new Interval(95, 105)
    val cGLSInVInterval = new Interval(0.20216, 0.22344)
    val TInterval = new Interval(80, 120)
    val ABPInterval = new Interval(0, 5)
    val ABInterval = new Interval(0.23, 0.27)
    val TextureInterval = new Interval(2.6, 3.0)

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
      case true =>  {
        //TODO: Delete. Debug
        println("Number of guesses for initial point: " + numberOfGuesses)
        (guessPoint, guessControl)
      }

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


  def captureTube(s: Int, rng: Random): Node = {
    def outputFile(s: Int) = dir + "depth" + maxDepth + "kdTree" + s  + "step" + timeStep + ".csv"

    def initialSlice = {
      val firstSlice = FirstKdTree.firstKdTree(rng)
      deleteFile(outputFile(1))
      val output: Output = Resource.fromFile(outputFile(1))
      kdTreeToFile(firstSlice, output, "intervals+control")
      println("First slice created.")
      println("Normalized volume: " + firstSlice.volumeKdTreeNormalized(explorationZone))
      firstSlice
    }

    (2 to s).foldLeft(initialSlice) {
      (slice, step) =>
        def currentIFunction: RichIndicatorFunction = targetIFunctionCreation(slice, model)
        val initNode = initialNodeCreation(currentIFunction, rng)
        val newSlice = kdTreeComputation(initNode, maxDepth, currentIFunction, rng)
        deleteFile(outputFile(step))
        val output: Output = Resource.fromFile(outputFile(step))
        kdTreeToFile(newSlice, output, "intervals+control")
        println("One slice created.")
        println("Normalized volume: " + newSlice.volumeKdTreeNormalized(explorationZone))
        //println(diff(newSlice, slice) + "," + (diff(slice, newSlice)))
        newSlice
    }
  }




  // TODO: Consider true intersection: if a cube intersects the boundary of the initialization zone we should cut it (not remove it)
  def strongIntersectionWithInitializationZone(tree: TreeSet[ZoneAndControl], initZone: Zone): TreeSet[ZoneAndControl] = {
    def oneIntervalNotContained(z: ZoneAndControl): Boolean = {
      val indexedIntervals = z.zone.region.zipWithIndex
      indexedIntervals.exists(x => x._1.min  < initZone.region(x._2).min || x._1.max  > initZone.region(x._2).max)
    }
    tree.filterNot( x => oneIntervalNotContained(x))

  }

  // TODO: Consider true intersection: if a cube intersects the boundary of the initialization zone we should cut it (not keep it)
  def weakIntersectionWithInitializationZone(tree: TreeSet[ZoneAndControl], initZone: Zone): TreeSet[ZoneAndControl] = {
    def oneIntervalNotIntersects(z: ZoneAndControl): Boolean = {
      val indexedIntervals = z.zone.region.zipWithIndex
      indexedIntervals.exists(x => x._1.min  > initZone.region(x._2).max || x._1.max  < initZone.region(x._2).min)
    }
    tree.filter(x => oneIntervalNotIntersects(x))

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
    captureTube(6, new Random(3))
    println("Depth " + maxDepth)


    // TESTS
    //val test = FirstKdTree.firstKdTree(new Random(3)).volumeKdTreeNormalized(explorationZone)
    //println("Normalized volume: " + test)
    //val depth = 10
    //println(kdTreeComputation(root, depth, iFunctionCircle(), new Random(3)).volumeKdTree  + "  depth = " + depth)








  }





}


