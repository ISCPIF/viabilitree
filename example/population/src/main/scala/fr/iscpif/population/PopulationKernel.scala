/**
 * Created with IntelliJ IDEA.
 * User: ia
 * Date: 30/09/13
 * Time: 10:38
 * To change this template use File | Settings | File Templates.
 */
package fr.iscpif.population

import fr.iscpif.viability._
import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.content._
import fr.iscpif.kdtree.export._
import scala.util.Random
import scalax.io.Resource
import math._

object PopulationKernel extends App with OracleApproximation with ZoneAndPointInput {
  val a = 0.2
  val b = 3.0
  val c = 0.5
  val d = -2.0
  val e = 2.0
  /*
for these parameters, for every value of x in [a,b], there is a value of y such that (x,y) is in the kernel
 */
  /*
  def oracle(p: _root_.fr.iscpif.kdtree.structure.Point): Boolean = {
    p(0)>= a && p(0)<= b &&
      p(1)>= -sqrt(2*c*log(b/p(0)))  &&    p(1)<= sqrt(2*c*log(p(0)/a))
  }
*/

  def oracle(p: _root_.fr.iscpif.kdtree.structure.Point): Boolean = {
    p(0) >= a && p(0) <= b &&
      p(1) <= sqrt(2 * c * log(b / p(0))) && p(1) >= -sqrt(2 * c * log(p(0) / a))
  }

  def zone: Zone = Seq((a, b), (d, e))

  def point = Seq(1.0, 0.0)

  def depth: Int = 18

  saveVTK2D(apply.get, Resource.fromFile(s"/tmp/population/kernelVTItest${depth}.vtk"))

}
