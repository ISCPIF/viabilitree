package fr.iscpif.population

/**
 * Created with IntelliJ IDEA.
 * User: ia
 * Date: 30/09/13
 * Time: 11:10
 * To change this template use File | Settings | File Templates.
 */

import fr.iscpif.viability._
import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.content._
import fr.iscpif.kdtree.visualisation._
import scala.util.Random
import scalax.io.Resource

object PopulationViability extends App
    with ViabilityKernel
    with ZoneInput
    with ParallelEvaluator
    with GridSampler {
  def a = 0.2
  def b = 3.0
  def c = 0.5
  def d = -2.0
  def e = 2.0

  def depth: Int = 16

  def zone = Seq((a, b), (d, e))
  //  def zone = Seq((a-0.01, b+0.01), (d-0.01, e+0.01))

  // def zone = Seq((0.2, 3.0), (-2.0, 2.0))
  //  def zone = Seq((0.0, 2.0), (0.0, 3.0))

  def dynamic(point: Point, control: Point) = Population(point, control)
  def controls = (-0.5 to 0.5 by 0.002).map(Seq(_))

  def dimension = 2

  def initialZone = zone

  implicit lazy val rng = new Random(42)

  //TODO replace when debug is over
  /*
  for {
    (b, s) <- apply.zipWithIndex
  } {
    println(s)
    b.saveVTK2D(Resource.fromFile(s"/tmp/population/populationGRID${depth}s$s.vtk"))
  }
*/

  val listeResult = apply.zipWithIndex
  listeResult.foreach {
    case (b, s) =>
      //if (listeResult.hasNext && (s % 10 != 0)) println("on passe")
      //else {
        println(s"step $s")
        b.saveVTK2D(Resource.fromFile(s"/tmp/population/population${depth}ts0.01s${s}.vtk"))
      //}
  }

}
