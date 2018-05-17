package viabilitree

import viability.kernel._
import kdtree._
import viabilitree.model.Control

package object strategy {

  def findViableControl(
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    point: Vector[Double],
    controls: Vector[Control],
    oracle: Vector[Double] => Boolean) =
    controls.find { c => oracle(dynamic(point, c.value)) }.map(_.value)

  def exhaustiveStrategy(kc: KernelComputation, k: viability.kernel.Kernel, point: Vector[Double]): Option[Vector[Double]] =
    k match {
      case _: EmptyTree[_] => None
      case k: NonEmptyTree[KernelContent] => findViableControl(kc.dynamic, point, kc.controls(point), k.contains)
    }

  def basicStrategy(kc: KernelComputation, k: viability.kernel.Kernel, point: Vector[Double]): Option[Vector[Double]] = {
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

        basicControl orElse exhaustiveStrategy(kc, k, point)
    }
  }

}
