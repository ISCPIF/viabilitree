package viabilitree

import viability.kernel._
import kdtree._
import viabilitree.model.Control

import com.thoughtworks.xstream._
import io.binary._
import better.files._
import cats._
import cats.implicits._

package object strategy {

  case class StrategyElement(point: Vector[Double], control: Vector[Double])

  case class StrategyExploration(guess: Option[Vector[Control] => Option[Vector[Double]]] = None)

  /*
  def mostDemandingControl(lesC: Vector[Control]):Option[Vector[Double]] = {
    if (lesC.isEmpty)  None else {
    }
  }

  def mostDemandingControl : Vector[Control] => Option[Vector[Double]]= None
*/

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

    def unrollStrategyPB(remainingSteps: Int, point: Vector[Double], acc: List[StrategyElement]): Vector[StrategyElement] = {
      if (remainingSteps == 0) acc.reverse.toVector
      else {
        acc.reverse.toVector
      }
    }

    unrollStrategy0(steps, point)
  }

  def evolution(
    point: Vector[Double],
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    strategy: Vector[Double] => Option[Vector[Double]],
    steps: Int) = {
    val xut = unrollStrategy(point, dynamic, strategy, steps)
    xut.map(_.point)
  }

  def cevolution(
    point: Vector[Double],
    dynamic: (Vector[Double], Vector[Double]) => Vector[Double],
    strategy: Vector[Double] => Option[Vector[Double]],
    steps: Int) = {
    val xut = unrollStrategy(point, dynamic, strategy, steps)
    val trajt = xut.map(_.point)
    val controlt = xut.map(_.control)
    (trajt, controlt)
  }

  def exhaustiveStrategy(dynamic: (Vector[Double], Vector[Double]) => Vector[Double], controls: Vector[Control], oracle: Vector[Double] => Boolean)(point: Vector[Double]): Option[Vector[Double]] =
    controls.find { c => oracle(dynamic(point, c.value)) }.map(_.value)

  def exhaustiveStrategy(kc: KernelComputation, k: viability.kernel.Kernel)(point: Vector[Double]): Option[Vector[Double]] =
    k match {
      case _: EmptyTree[_] => None
      case k: NonEmptyTree[KernelContent] => exhaustiveStrategy(kc.dynamic, kc.controls(point), k.contains)(point)
    }

  /* def exhaustiveStrategyGuess(dynamic: (Vector[Double], Vector[Double]) => Vector[Double], controls: Vector[Control], oracle: Vector[Double] => Boolean, guess: Vector[Control] => Option(Vector[Double]))(point: Vector[Double]): Option[Vector[Double]] = {
    val cViable = controls.find { c => oracle(dynamic(point, c.value)) }.map(_.value)
    val result = if cViable.isDefined cViable else guess(controls)
  }

  def exhaustiveStrategyGuess(kc: KernelComputation, k: viability.kernel.Kernel)(point: Vector[Double]): Option[Vector[Double]] =
    k match {
      case _: EmptyTree[_] => None
      case k: NonEmptyTree[KernelContent] => exhaustiveStrategyGuess(kc.dynamic, kc.controls(point), k.contains, min)(point)
    }

*/
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

  def constantStrategy(u: Option[Control] = None)(point: Vector[Double]): Option[Vector[Double]] = {
    u match {
      case None => Some(Vector(0.0))
      case _ => Some(u.get.value)
    }
  }

  def basicGuessStrategy(kc: KernelComputation, k: viability.kernel.Kernel)(point: Vector[Double]): Option[Vector[Double]] = {
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

  def traceStrategyElemTraj(t: Seq[StrategyElement], file: File): Unit = {
    file.delete(true)
    file.parent.createDirectories()
    file.touch()
    t.foreach { s =>
      file.append(s.point.mkString(" "))
      file.append(" ")
      file.append(s.control.mkString(" "))
      file.append(s"\n")
    }
  }

}
