/**
 * Created with IntelliJ IDEA.
 * User: ia
 * Date: 30/09/13
 * Time: 10:38
 * To change this template use File | Settings | File Templates.
 */
package fr.iscpif.population

import viabilitree.export._
import viabilitree.approximation._

import scala.util.Random
import math._

object PopulationApproximation extends App {

  val a = 0.2
  val b = 3.0
  val c = 0.5
  val d = -2.0
  val e = 2.0

  def oracle(p: Vector[Double]): Boolean = {
    p(0) >= a && p(0) <= b &&
      p(1) <= sqrt(2 * c * log(b / p(0))) && p(1) >= -sqrt(2 * c * log(p(0) / a))
  }

  val depth = 18

  val approximation =
    OracleApproximation(
      depth = depth,
      box =
        Vector((a, b), (d, e)),
      oracle = oracle,
      point = Option(Vector(1.0, 0.0)))

  implicit val random = new Random(42)
  val res = approximate(approximation).get

  saveVTK2D(res, s"/tmp/population/kernelVFtest${depth}.vtk")
}

