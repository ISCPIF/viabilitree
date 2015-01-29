/**
 * Created with IntelliJ IDEA.
 * User: ia
 * Date: 13/03/14
 * Time: 21:46
 * To change this template use File | Settings | File Templates.
 */

package fr.iscpif.bilingual


import fr.iscpif.viability._
import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.content._
import fr.iscpif.kdtree.visualisation._
import scala.util.Random
import scalax.io.Resource
import math._
import fr.iscpif.viability.kernel._
import fr.iscpif.viability.control._

object BilingualDomain extends App
with ViabilityKernel
with ZoneInput
with LearnK
// with ParallelEvaluator
with GridSampler {

  def k(p: Point) = {
    p(0) <= 1 && p(0) >= 0.2 && p(1) <= 1 && p(1) >= 0.2   &&  (p(0)+p(1) <=1 ) && p(2)<=1 && p(2)>=0
  }

  def zone = Seq(0.2 -> 1.0, 0.2 -> 1.0, 0.0 -> 1.0)

  override def domain = Seq(0.0 -> 1.0, 0.0 -> 1.0, 0.0 -> 1.0)

  def depth = 15

  def dynamic(point: Point, control: Point) = Bilingual(point, control)

  def controls = (-0.1 to 0.1 by 0.005).map(Control(_))

  def dimension = 3

//  def initialZone = zone

  implicit lazy val rng = new Random(42)

  val it = apply.zipWithIndex
  it.foreach {
    case (b, s) =>
      println(s"step $s")
      if (s % 10 == 0 || !it.hasNext) b.saveVTK3D(Resource.fromFile(s"/tmp/bilingual/domainBilingual${depth}s$s.vtk"))
  }
}
