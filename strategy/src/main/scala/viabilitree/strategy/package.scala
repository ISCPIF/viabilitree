package viabilitree

import viability.kernel._
import kdtree._
import viabilitree.model.Control

package object strategy {

  case class StrategyElement(point: Vector[Double], control: Vector[Double])

  def unrollStrategy(
    point: Vector[Double],
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    // strategy is able to give for a point a viable control (or None if None)
    // an example of use is: basicStrategy(kc,k)
    strategy: Vector[Double] => Option[Vector[Double]],
    steps: Int) = {

    def unrollStrategy0(remainingSteps: Int, point: Vector[Double], acc: List[StrategyElement] = List.empty): Vector[StrategyElement] =
      if (remainingSteps == 0) acc.reverse.toVector
      else {
        strategy(point) match {
          case None => acc.reverse.toVector
          case Some(control) => unrollStrategy0(remainingSteps - 1, dynamic(point, control), StrategyElement(point, control) :: acc)
        }
      }

    unrollStrategy0(steps, point)
  }

  def evolution(   point: Vector[Double],
                   dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
                   strategy: Vector[Double] => Option[Vector[Double]],
                   steps: Int) = {
    val xut = unrollStrategy(point,dynamic,strategy,steps)
    xut.map(_.point)
  }


  def exhaustiveStrategy(dynamic: (Vector[Double], Vector[Double]) => Vector[Double], controls: Vector[Control], oracle: Vector[Double] => Boolean)(point: Vector[Double]): Option[Vector[Double]] =
    controls.find { c => oracle(dynamic(point, c.value)) }.map(_.value)

  def exhaustiveStrategy(kc: KernelComputation, k: viability.kernel.Kernel)(point: Vector[Double]): Option[Vector[Double]] =
    k match {
      case _: EmptyTree[_] => None
      case k: NonEmptyTree[KernelContent] => exhaustiveStrategy(kc.dynamic, kc.controls(point), k.contains)(point)
    }

  def basicStrategy(kc: KernelComputation, k: viability.kernel.Kernel)(point: Vector[Double]): Option[Vector[Double]] = {
    k match {
      case _: EmptyTree[_] => None
      case k: NonEmptyTree[KernelContent] =>
        val pointControls = kc.controls(point)

        val basicControl =
          for {
            leaf <- k.containingLeaf(point)
            testPointControls = kc.controls(leaf.content.testPoint)
            testPointControlIndex <- leaf.content.control
            testPointControlValue = testPointControls(testPointControlIndex)
            basicControl <- pointControls.find(_ == testPointControlValue)
          } yield basicControl.value

        basicControl orElse exhaustiveStrategy(kc, k)(point)
    }
  }

}
