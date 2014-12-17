package fr.iscpif.population

import fr.iscpif.viability._
import fr.iscpif.kdtree.algorithm._
import fr.iscpif.kdtree.structure._
import fr.iscpif.kdtree.content._
import fr.iscpif.kdtree.visualisation._
import scala.util.Random
import scalax.io.Resource
import fr.iscpif.viability._
import kernel._
import control._

object PopulationViability extends App
    with ViabilityKernel
    with ZoneInput
    with ZoneK
    with GridSampler {
  def a = 0.2
  def b = 3.0
  def c = 0.5
  def d = -2.0
  def e = 2.0

  def depth: Int = 18

  def zone = Seq((a, b), (d, e))
  //  def zone = Seq((a-0.01, b+0.01), (d-0.01, e+0.01))

  // def zone = Seq((0.2, 3.0), (-2.0, 2.0))
  //  def zone = Seq((0.0, 2.0), (0.0, 3.0))

  def dynamic(point: Point, control: Point) = Population(point, control)
  lazy val controls = (-0.5 to 0.5 by 0.02).map(Control(_))

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

  val begin = System.currentTimeMillis()

  val listeResult = apply.zipWithIndex
  listeResult.foreach {
    case (b, s) =>
      //if (listeResult.hasNext && (s % 10 != 0)) println("on passe")
      //else {
      println(s"step $s")
      if (s % 20 == 0 || !listeResult.hasNext) b.saveVTK2D(Resource.fromFile(s"/tmp/population/population${s}.vtk"))
    //}
  }

  println(System.currentTimeMillis - begin)

}
