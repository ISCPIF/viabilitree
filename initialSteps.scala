package fr.iscpif.viability

import fr.iscpif.viability.kdtreeBounded._
import fr.iscpif.viability._
import fr.iscpif.viability.viabilityLanguages._
import fr.iscpif.viability.LanguageModel._

import scala.collection.immutable.TreeSet
import scala.math.Ordering.Implicits._
import scala.math._


 package object initialSteps {

  val dataLanguagesPath = "data-targetLanguages.dat"

  object InputProcessing {
    import fr.iscpif.viability.viabilityLanguages._


    trait TargetPoint{
      // The original grid divides [0, 1] from 0 to 99, i.e. (1, 1, 1) = "99 99 99".
      // [pointName] refers to the right part of the equality
      val pointName: List[Int]
      val distanceToBoundary: Double
    }

    def fileToIteratorString(): Iterator[String] = {
      val source = scala.io.Source.fromFile(dataLanguagesPath)
      val lines = source.getLines()
      val targetIterator: Iterator[String] = lines.drop(1)
      //source.close()
      targetIterator
    }


    def dataFormatToTargetPoint(line: String): TargetPoint = {
      def parseDouble(s: String): Option[Double] = try{ Some(s.toDouble) } catch{ case _: Throwable => None }
      def parseInt(s: String): Option[Int] = try{ Some(s.toInt) } catch{ case _: Throwable => None }

      val array = line.split(" ")
      val newPoint: List[Int] =
        (parseInt(array(0)), parseInt(array(1)), parseInt(array(2))) match {
          case ((Some(x: Int), Some(y: Int), Some(s: Int))) => List(x, y, s)
          case _ => throw new RuntimeException("Input target error.")
        }
      val newDistance: Double =
        parseDouble(array(3)) match {
          case Some(d) => d
          case _ => throw new RuntimeException("Input target error.")
        }
      new TargetPoint {
        val pointName: List[Int] = newPoint
        val distanceToBoundary: Double = newDistance
      }
    }

    def linesToTargetPoints(targetLines: Iterator[String]): Iterator[TargetPoint] =
      targetLines.map(x => dataFormatToTargetPoint(x))


    // TODO: Delete. Debug
    ////// TEST
    object TestInputs{
      val p1 = new TargetPoint {
        val distanceToBoundary: Double = 0
        val pointName: List[Int] = List(47, 19, 99)
      }
      val p2 = new TargetPoint {
        val distanceToBoundary: Double = 4
        val pointName: List[Int] = List(5, 2, 8)
      }


      val iteratorString = fileToIteratorString()
      val targetPointsIterator = linesToTargetPoints(iteratorString)
      val treeSet = treeFormattingRich(targetPointsIterator)

      var projectionTreeSet: TreeSet[List[Int]] = new TreeSet[List[Int]]()(LexicographicalOrderInt)
      treeSet.foreach(x => { projectionTreeSet = projectionTreeSet + x.pointName})

      val stateTest: State = Array(47.0/99, 19.0/99, 0.0/99)

    }
    /////////

    object LexicographicalOrderTP extends Ordering[TargetPoint] {
      def compare(x: TargetPoint, y: TargetPoint) = {
        val xTuple = (x.pointName(0), x.pointName(1), x.pointName(2))
        val yTuple = (y.pointName(0), y.pointName(1), y.pointName(2))
        if(xTuple < yTuple) -1
        else if(yTuple < xTuple) 1
        else 0
      }
    }

    object LexicographicalOrderInt extends Ordering[List[Int]] {
      def compare(x: List[Int], y: List[Int]) = {
        val xTuple = (x(0), x(1), x(2))
        val yTuple = (y(0), y(1), y(2))
        if(xTuple < yTuple) -1
        else if(yTuple < xTuple) 1
        else 0
      }
    }

    def treeFormattingRich(targetDescription: Iterator[TargetPoint]): TreeSet[TargetPoint] = {
      val treeSet: TreeSet[TargetPoint] = new TreeSet[TargetPoint]()(LexicographicalOrderTP)
      treeSet ++ targetDescription
    }

    def treeFormattingLight(targetDescription: Iterator[List[Int]]): TreeSet[List[Int]] = {
      val treeSet: TreeSet[List[Int]] = new TreeSet[List[Int]]()(LexicographicalOrderInt)
      treeSet ++ targetDescription
    }



  }


  object FirstKdTree {
    import fr.iscpif.viability.viabilityLanguages._
    import InputProcessing._



    // [point] belongs to [0,1]^3. If it is not the case it should have been discarded previously
    def closestGridPoint(point: State) : List[Int] = {
      //TODO: Delete. Debug
      //println("Computing th closest point to " + point.toList)
      assume(point.length == 3)
      point.foreach(x => assume( 0 <= x && x<= 1))

      val pointName = List(point(0)*99, point(1)*99, point(2)*99)
      val xGridPoints = (pointName(0).floor.toInt, pointName(0).ceil.toInt)
      val yGridPoints = (pointName(1).floor.toInt, pointName(1).ceil.toInt)
      val sGridPoints = (pointName(2).floor.toInt, pointName(2).ceil.toInt)

      def projection(tuple: (Int, Int), coordinate: Int): Int = {
        if (coordinate == 0) tuple._1
        else if (coordinate == 1) tuple._2
        else throw new RuntimeException("Only projections on coordinate 0 or 1 are admitted.")
      }
      def auxFunction(x: Double): Int = {
        if(0 <= x && x < 0.5) 0
        else if(0.5 <= x && x < 1) 1
        else throw new RuntimeException("This point should belong to [0,1).")
      }
      val closestX: Int = auxFunction(pointName(0) - pointName(0).floor)
      val closestY: Int = auxFunction(pointName(1) - pointName(1).floor)
      val closestS: Int = auxFunction(pointName(2) - pointName(2).floor)

      val closestGridPoint =
        List(projection(xGridPoints, closestX), projection(yGridPoints,closestY), projection(sGridPoints,closestS))

      closestGridPoint
    }


    def targetToIFunction(): RichIndicatorFunction = {
      val iteratorString = fileToIteratorString()
      val targetPointsIterator = linesToTargetPoints(iteratorString)
      val treeSet = treeFormattingRich(targetPointsIterator)

      var projectionTreeSet: TreeSet[List[Int]] = new TreeSet[List[Int]]()(LexicographicalOrderInt)
      treeSet.foreach(x => {
        projectionTreeSet = projectionTreeSet + x.pointName
      })

      //If the point is not in the definition domain we don't even execute the model
      {state: State =>
        if (state.sigmaA + state.sigmaB > 1) None
        else {
          var k = 0
          val guessControlArray: Array[Double] = controlTestOrder(validControlInterval(state))
          var control: Option[Double] = None
          var controlFound = false
          while(k< numberOfControlTests && !controlFound){
            val image = model(state, guessControlArray(k))
            //TODO: Delete. Debug.
            //println("State: " + state.toList + "  Guess control: " + guessControlArray(k) + "  Image: " + image.toList)
            if (projectionTreeSet.contains(closestGridPoint(image)) == true) {
              println("THE IMAGE IS IN THE TARGET")
              control =  Some(guessControlArray(k))
              controlFound = true
            }
            else println("NOT IN THE TARGET")
            k +=1
          }
          control
        }
      }

    }

    val targetIFunction: RichIndicatorFunction = targetToIFunction()
    //TODO: Change to val? is it OK randomNG as input?
    def initialNode: Node = initialNodeCreation(targetIFunction: RichIndicatorFunction)(randomNG)
    def firstKdTree: Node = {println("I'VE BEEN HERE 2") ; kdTreeComputation(initialNode, maxDepth, targetIFunction)(randomNG)}

  }

}